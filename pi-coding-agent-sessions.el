;;; pi-coding-agent-sessions.el --- Project and session browser for Pi -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Cross-project session discovery and navigation for pi-coding-agent.
;; Provides a tabulated ledger and a completion-based session switcher.
;; Live chat buffers are merged with recent JSONL session history.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'vc)
(require 'pi-coding-agent-menu)

(declare-function pi-coding-agent "pi-coding-agent" (&optional session))
(declare-function pi-coding-agent-open-session-file "pi-coding-agent" (session-file))
(declare-function pi-coding-agent--show-session-buffers "pi-coding-agent"
                  (chat-buf input-buf))

(defgroup pi-coding-agent-sessions nil
  "Cross-project Pi session navigation."
  :group 'pi-coding-agent
  :prefix "pi-coding-agent-sessions-")

(defun pi-coding-agent-sessions--default-history-roots ()
  "Return the default local Pi session history roots."
  (list
   (expand-file-name
    "sessions/"
    (file-name-as-directory
     (or (getenv "PI_CODING_AGENT_DIR") "~/.pi/agent/")))))

(defcustom pi-coding-agent-sessions-history-roots
  (pi-coding-agent-sessions--default-history-roots)
  "Directories containing Pi session project directories.
Each directory may contain JSONL files directly or one directory per project.
Live sessions contribute their own history directory automatically, including
TRAMP directories."
  :type '(repeat directory)
  :group 'pi-coding-agent-sessions)

(defcustom pi-coding-agent-sessions-history-per-directory 12
  "Maximum recent JSONL files read from each session directory."
  :type 'natnum
  :group 'pi-coding-agent-sessions)

(defcustom pi-coding-agent-sessions-history-limit 200
  "Maximum historical sessions parsed for one catalog refresh."
  :type 'natnum
  :group 'pi-coding-agent-sessions)

(defcustom pi-coding-agent-sessions-show-header-overview t
  "Whether Pi chat headers show global live-session counts."
  :type 'boolean
  :group 'pi-coding-agent-sessions)

(defcustom pi-coding-agent-sessions-hide-missing-directories t
  "Whether to hide historical sessions whose local working directory is gone.
This keeps sessions from deleted Git worktrees or Jujutsu workspaces out of the
ledger.  Remote directories are not probed implicitly, to avoid unexpected
TRAMP connections."
  :type 'boolean
  :group 'pi-coding-agent-sessions)

(defcustom pi-coding-agent-sessions-sort-order 'project
  "Default ordering for the ledger and session switcher.
`project' groups sessions by their native Emacs project, with active work first
inside each project.  `attention' puts all active work first across projects.
`recent' sorts only by last activity."
  :type '(choice (const :tag "Project" project)
                 (const :tag "Attention" attention)
                 (const :tag "Recent" recent))
  :group 'pi-coding-agent-sessions)

(defface pi-coding-agent-sessions-working
  '((t :inherit warning))
  "Face for working sessions."
  :group 'pi-coding-agent-sessions)

(defface pi-coding-agent-sessions-finished
  '((t :inherit success :weight bold))
  "Face for completed sessions not yet opened."
  :group 'pi-coding-agent-sessions)

(defface pi-coding-agent-sessions-ready
  '((t :inherit success))
  "Face for ready live sessions."
  :group 'pi-coding-agent-sessions)

(defface pi-coding-agent-sessions-history
  '((t :inherit shadow))
  "Face for historical sessions."
  :group 'pi-coding-agent-sessions)

(defface pi-coding-agent-sessions-stopped
  '((t :inherit error))
  "Face for stopped live sessions."
  :group 'pi-coding-agent-sessions)

(defface pi-coding-agent-sessions-project-heading
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for project headings in the Agent Ledger."
  :group 'pi-coding-agent-sessions)

(defconst pi-coding-agent-sessions-buffer-name "*Pi Agent Ledger*"
  "Name of the Pi project and session ledger buffer.")

(defvar pi-coding-agent-sessions--metadata-cache (make-hash-table :test #'equal)
  "Cache mapping session paths to file signatures and parsed metadata.")

(defvar pi-coding-agent-sessions--historical-records nil
  "Historical records from the most recent catalog history scan.")

(defvar pi-coding-agent-sessions--history-loaded-p nil
  "Whether local session history has been loaded into the catalog cache.")

(defvar pi-coding-agent-sessions--observed-remote-history-directories
  (make-hash-table :test #'equal)
  "Remote history directories learned from live Pi sessions.")

(defvar pi-coding-agent-sessions--finished-buffers
  (make-hash-table :test #'eq :weakness 'key)
  "Live chat buffers that completed since they were last opened.")

(defvar pi-coding-agent-sessions--refresh-timer nil
  "Pending debounced ledger refresh timer.")

(defvar-local pi-coding-agent-sessions--records nil
  "Hash table mapping ledger row ids to session records.")

(defvar-local pi-coding-agent-sessions--delete-marks nil
  "Hash table containing ledger session ids marked for deletion.")

(defvar pi-coding-agent-sessions--project-root-cache
  (make-hash-table :test #'equal)
  "Cache of native project roots, including remote project lookups.")

(defvar pi-coding-agent-sessions--working-tree-relation-cache
  (make-hash-table :test #'equal)
  "Cache of VC working-tree relations, including remote backend queries.")

(defun pi-coding-agent-sessions--ensure-caches ()
  "Initialize catalog caches that may be nil after hot reloading older code."
  (unless (hash-table-p pi-coding-agent-sessions--observed-remote-history-directories)
    (setq pi-coding-agent-sessions--observed-remote-history-directories
          (make-hash-table :test #'equal)))
  (unless (hash-table-p pi-coding-agent-sessions--project-root-cache)
    (setq pi-coding-agent-sessions--project-root-cache
          (make-hash-table :test #'equal)))
  (unless (hash-table-p pi-coding-agent-sessions--working-tree-relation-cache)
    (setq pi-coding-agent-sessions--working-tree-relation-cache
          (make-hash-table :test #'equal))))

(defun pi-coding-agent-sessions--chat-buffers ()
  "Return all live Pi chat buffers in buffer recency order."
  (cl-remove-if-not
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (derived-mode-p 'pi-coding-agent-chat-mode))))
   (buffer-list)))

(defun pi-coding-agent-sessions--file-signature (path)
  "Return a cache signature for session file PATH, or nil."
  (when-let* ((attrs (ignore-errors (file-attributes path))))
    (list (file-attribute-modification-time attrs)
          (file-attribute-size attrs))))

(defun pi-coding-agent-sessions--metadata (path)
  "Return cached session metadata for PATH."
  (when-let* ((signature (pi-coding-agent-sessions--file-signature path)))
    (let ((cached (gethash path pi-coding-agent-sessions--metadata-cache)))
      (if (equal signature (car-safe cached))
          (cdr cached)
        (let ((metadata (pi-coding-agent--session-metadata path)))
          (puthash path (cons signature metadata)
                   pi-coding-agent-sessions--metadata-cache)
          metadata)))))

(defun pi-coding-agent-sessions--native-project-root (directory)
  "Return Emacs' canonical project root for DIRECTORY.
Fall back to DIRECTORY when no project backend recognizes it."
  (let* ((directory (pi-coding-agent--normalize-directory directory))
         (missing (make-symbol "missing"))
         (cached (if (hash-table-p pi-coding-agent-sessions--project-root-cache)
                     (gethash directory
                              pi-coding-agent-sessions--project-root-cache
                              missing)
                   missing)))
    (if (not (eq cached missing))
        cached
      (let* ((project (ignore-errors (project-current nil directory)))
             (root
              (or (and project
                       (condition-case nil
                           (project-root project)
                         (cl-no-applicable-method
                          (and (consp project) (stringp (cdr project))
                               (cdr project)))))
                  directory))
             (normalized (pi-coding-agent--normalize-directory root)))
        (when (hash-table-p pi-coding-agent-sessions--project-root-cache)
          (puthash directory normalized
                   pi-coding-agent-sessions--project-root-cache))
        normalized))))

(defun pi-coding-agent-sessions--shorter-directory-p (a b)
  "Return non-nil when directory A is a better group root than B."
  (let ((a-local (or (file-remote-p a 'localname) a))
        (b-local (or (file-remote-p b 'localname) b)))
    (if (= (length a-local) (length b-local))
        (string-lessp a-local b-local)
      (< (length a-local) (length b-local)))))

(defun pi-coding-agent-sessions--known-working-tree-roots (project-root)
  "Return PROJECT-ROOT and related roots reported through generic VC.
Cached results are tied to the resolved backend function, so loading a VC
backend extension invalidates an earlier result that lacked the operation."
  (let* ((backend (ignore-errors (vc-responsible-backend project-root)))
         (function
          (and backend
               (vc-find-backend-function
                backend 'known-other-working-trees)))
         (cached
          (and (hash-table-p
                pi-coding-agent-sessions--working-tree-relation-cache)
               (gethash project-root
                        pi-coding-agent-sessions--working-tree-relation-cache))))
    (if (and (eq (car-safe cached) :function)
             (equal (plist-get cached :function) function))
        (plist-get cached :roots)
      (let* ((other-roots
              (and function
                   (let ((default-directory project-root))
                     (ignore-errors
                       (if (consp function)
                           (apply (car function) (cdr function))
                         (funcall function))))))
             (roots
              (delete-dups
               (delq nil
                     (mapcar
                      (lambda (root)
                        (condition-case nil
                            (pi-coding-agent--normalize-directory
                             (pi-coding-agent--emacs-directory
                              root project-root))
                          (error nil)))
                      (cons project-root other-roots))))))
        (when (hash-table-p pi-coding-agent-sessions--working-tree-relation-cache)
          (puthash project-root
                   (list :function function :roots roots)
                   pi-coding-agent-sessions--working-tree-relation-cache))
        roots))))

(defun pi-coding-agent-sessions--apply-working-tree-groups (records)
  "Group RECORDS connected by VC's known-working-tree relation.
Only roots represented by session records participate, so unrelated or stale
workspace paths cannot become ledger headings."
  (let ((records-by-root (make-hash-table :test #'equal))
        (neighbors (make-hash-table :test #'equal))
        (visited (make-hash-table :test #'equal)))
    (dolist (record records)
      (push record
            (gethash (plist-get record :project-root) records-by-root)))
    (maphash
     (lambda (root _records)
       (dolist (other (pi-coding-agent-sessions--known-working-tree-roots root))
         (when (gethash other records-by-root)
           (push other (gethash root neighbors))
           (push root (gethash other neighbors)))))
     records-by-root)
    (maphash
     (lambda (root _records)
       (unless (gethash root visited)
         (let ((pending (list root))
               (component nil))
           (while pending
             (let ((current (pop pending)))
               (unless (gethash current visited)
                 (puthash current t visited)
                 (push current component)
                 (setq pending
                       (append (gethash current neighbors) pending)))))
           (let ((canonical
                  (car (sort component
                             #'pi-coding-agent-sessions--shorter-directory-p))))
             (dolist (member-root component)
               (dolist (record (gethash member-root records-by-root))
                 (plist-put record :project-root canonical)
                 (plist-put record :project-name
                            (pi-coding-agent-sessions--project-name
                             canonical))))))))
     records-by-root)
    records))

(defun pi-coding-agent-sessions--project-name (root)
  "Return a concise display name for project ROOT."
  (let* ((local (or (file-remote-p root 'localname) root))
         (name (file-name-nondirectory (directory-file-name local))))
    (if (string-empty-p name) root name)))

(defun pi-coding-agent-sessions--project-display-name (record)
  "Return a route-aware project label for RECORD."
  (let* ((name (plist-get record :project-name))
         (root (plist-get record :project-root))
         (remote (and root (pi-coding-agent--remote-prefix root))))
    (if remote
        (format "%s @ %s"
                name
                (string-remove-suffix ":"
                                      (string-remove-prefix "/" remote)))
      name)))

(defun pi-coding-agent-sessions--model-name (state)
  "Return the model display name from session STATE."
  (let ((model (plist-get state :model)))
    (cond
     ((stringp model) model)
     ((plist-get model :name))
     (t ""))))

(defun pi-coding-agent-sessions--live-status (chat-buffer)
  "Return catalog status for live CHAT-BUFFER."
  (with-current-buffer chat-buffer
    (let ((process pi-coding-agent--process))
      (cond
       ((or (not (processp process)) (not (process-live-p process))) 'stopped)
       ((gethash chat-buffer pi-coding-agent-sessions--finished-buffers) 'finished)
       ((or (not (eq pi-coding-agent--status 'idle))
            (not (equal pi-coding-agent--activity-phase "idle")))
        'working)
       (t 'ready)))))

(defun pi-coding-agent-sessions--one-line (text)
  "Collapse whitespace in TEXT so it is safe in table and completion rows."
  (when (stringp text)
    (replace-regexp-in-string
     "[[:space:]\n\r\t]+" " " (string-trim text))))

(defun pi-coding-agent-sessions--session-label (metadata fallback)
  "Return a one-line display label from METADATA, then FALLBACK."
  (or (pi-coding-agent-sessions--one-line
       (plist-get metadata :session-name))
      (pi-coding-agent-sessions--one-line
       (plist-get metadata :first-message))
      (pi-coding-agent-sessions--one-line fallback)
      "[empty session]"))

(defun pi-coding-agent-sessions--live-record (chat-buffer)
  "Return a catalog record for live CHAT-BUFFER."
  (with-current-buffer chat-buffer
    (let* ((directory (pi-coding-agent--chat-session-directory chat-buffer))
           (root (pi-coding-agent-sessions--native-project-root directory))
           (state pi-coding-agent--state)
           (path (plist-get state :session-file))
           (metadata (and (stringp path)
                          (pi-coding-agent-sessions--metadata path)))
           (mtime (or (plist-get metadata :modified-time)
                      (and pi-coding-agent--state-timestamp
                           (seconds-to-time pi-coding-agent--state-timestamp))
                      (current-time)))
           (fallback (or pi-coding-agent--session-name
                         (pi-coding-agent--chat-session-name chat-buffer)))
           (id (if (stringp path)
                   (concat "file:" path)
                 (format "buffer:%s" (buffer-name chat-buffer)))))
      (list :id id
            :path (and (stringp path) path)
            :buffer chat-buffer
            :directory directory
            :project-root root
            :project-name (pi-coding-agent-sessions--project-name root)
            :label (pi-coding-agent-sessions--session-label metadata fallback)
            :message-count (or (plist-get metadata :message-count) 0)
            :modified-time mtime
            :model (pi-coding-agent-sessions--model-name state)
            :status (pi-coding-agent-sessions--live-status chat-buffer)))))

(defun pi-coding-agent-sessions--history-record (path)
  "Return a historical catalog record for session PATH, or nil."
  (when-let* ((metadata (pi-coding-agent-sessions--metadata path))
              (cwd (plist-get metadata :cwd))
              ((stringp cwd))
              ((not (string-empty-p cwd))))
    (let* ((directory (pi-coding-agent--emacs-directory cwd path))
           (root (pi-coding-agent-sessions--native-project-root directory)))
      (list :id (concat "file:" path)
            :path path
            :buffer nil
            :directory directory
            :project-root root
            :project-name (pi-coding-agent-sessions--project-name root)
            :label (pi-coding-agent-sessions--session-label metadata nil)
            :message-count (or (plist-get metadata :message-count) 0)
            :modified-time (plist-get metadata :modified-time)
            :model ""
            :status 'history))))

(defun pi-coding-agent-sessions--record-directory-available-p (record)
  "Return non-nil when historical RECORD should remain visible."
  (let ((directory (plist-get record :directory)))
    (or (not pi-coding-agent-sessions-hide-missing-directories)
        (not (stringp directory))
        (file-remote-p directory)
        (file-directory-p directory))))

(defun pi-coding-agent-sessions--live-history-directories ()
  "Return unique session directories advertised by live chat buffers.
Remember remote directories so an explicit later refresh can scan them even
when the originating chat buffer has since been closed."
  (let ((directories
         (delete-dups
          (delq nil
                (mapcar #'pi-coding-agent--session-list-directory
                        (pi-coding-agent-sessions--chat-buffers))))))
    (dolist (directory directories)
      (when (file-remote-p directory)
        (puthash directory t
                 pi-coding-agent-sessions--observed-remote-history-directories)))
    directories))

(defun pi-coding-agent-sessions--root-directories (root)
  "Return session directories immediately below history ROOT.
Include ROOT itself when it directly contains JSONL files."
  (when (and (stringp root) (file-directory-p root))
    (let* ((children (ignore-errors (directory-files root t directory-files-no-dot-files-regexp t)))
           (directories (cl-remove-if-not #'file-directory-p children))
           (has-jsonl (seq-some (lambda (path)
                                  (and (file-regular-p path)
                                       (string-suffix-p ".jsonl" path)))
                                children)))
      (if has-jsonl (cons root directories) directories))))

(defun pi-coding-agent-sessions--recent-files (directory)
  "Return recent JSONL files from session DIRECTORY."
  (let ((files (ignore-errors (directory-files directory t "\\.jsonl\\'" t))))
    (seq-take
     (sort files
           (lambda (a b)
             (time-less-p
              (or (file-attribute-modification-time (file-attributes b))
                  '(0 0 0 0))
              (or (file-attribute-modification-time (file-attributes a))
                  '(0 0 0 0)))))
     pi-coding-agent-sessions-history-per-directory)))

(defun pi-coding-agent-sessions--history-files (&optional include-remote)
  "Return bounded recent session files from configured and live roots.
When INCLUDE-REMOTE is nil, avoid TRAMP filesystem access.  Live sessions are
still learned as remote history sources for a later explicit refresh."
  (let* ((live-dirs (pi-coding-agent-sessions--live-history-directories))
         (roots
          (if include-remote
              pi-coding-agent-sessions-history-roots
            (cl-remove-if #'file-remote-p
                          pi-coding-agent-sessions-history-roots)))
         (configured-dirs
          (mapcan #'pi-coding-agent-sessions--root-directories roots))
         (remote-dirs
          (and include-remote
               (hash-table-keys
                pi-coding-agent-sessions--observed-remote-history-directories)))
         (directories
          (delete-dups
           (append configured-dirs
                   (if include-remote live-dirs
                     (cl-remove-if #'file-remote-p live-dirs))
                   remote-dirs)))
         (files (delete-dups
                 (mapcan #'pi-coding-agent-sessions--recent-files directories))))
    (seq-take
     (sort files
           (lambda (a b)
             (time-less-p
              (or (file-attribute-modification-time (file-attributes b))
                  '(0 0 0 0))
              (or (file-attribute-modification-time (file-attributes a))
                  '(0 0 0 0)))))
     pi-coding-agent-sessions-history-limit)))

(defun pi-coding-agent-sessions--status-rank (status)
  "Return sort rank for session STATUS."
  (pcase status
    ('working 0)
    ('finished 1)
    ('ready 2)
    ('history 3)
    ('stopped 4)
    (_ 5)))

(defun pi-coding-agent-sessions--newer-p (a b)
  "Return non-nil when catalog record A is newer than B."
  (time-less-p (or (plist-get b :modified-time) '(0 0 0 0))
               (or (plist-get a :modified-time) '(0 0 0 0))))

(defun pi-coding-agent-sessions--attention-less-p (a b)
  "Return non-nil when catalog record A needs attention before B."
  (let ((a-rank (pi-coding-agent-sessions--status-rank (plist-get a :status)))
        (b-rank (pi-coding-agent-sessions--status-rank (plist-get b :status))))
    (if (/= a-rank b-rank)
        (< a-rank b-rank)
      (pi-coding-agent-sessions--newer-p a b))))

(defun pi-coding-agent-sessions--project-less-p (a b)
  "Return non-nil when catalog record A sorts before B by project."
  (let ((a-project
         (downcase (pi-coding-agent-sessions--project-display-name a)))
        (b-project
         (downcase (pi-coding-agent-sessions--project-display-name b))))
    (if (equal a-project b-project)
        (pi-coding-agent-sessions--attention-less-p a b)
      (string-lessp a-project b-project))))

(defun pi-coding-agent-sessions--record-less-p (a b)
  "Return non-nil when catalog record A should sort before B."
  (pcase pi-coding-agent-sessions-sort-order
    ('attention (pi-coding-agent-sessions--attention-less-p a b))
    ('recent (pi-coding-agent-sessions--newer-p a b))
    (_ (pi-coding-agent-sessions--project-less-p a b))))

(defun pi-coding-agent-sessions--group-newest (group)
  "Return the newest record in project GROUP."
  (car (sort (copy-sequence group) #'pi-coding-agent-sessions--newer-p)))

(defun pi-coding-agent-sessions--group-attention (group)
  "Return the most attention-worthy record in project GROUP."
  (car (sort (copy-sequence group)
             #'pi-coding-agent-sessions--attention-less-p)))

(defun pi-coding-agent-sessions--group-less-p (a b)
  "Return non-nil when project group A should sort before B."
  (pcase pi-coding-agent-sessions-sort-order
    ('attention
     (let* ((a-best (pi-coding-agent-sessions--group-attention a))
            (b-best (pi-coding-agent-sessions--group-attention b))
            (a-rank (pi-coding-agent-sessions--status-rank
                     (plist-get a-best :status)))
            (b-rank (pi-coding-agent-sessions--status-rank
                     (plist-get b-best :status))))
       (if (/= a-rank b-rank)
           (< a-rank b-rank)
         (pi-coding-agent-sessions--newer-p
          (pi-coding-agent-sessions--group-newest a)
          (pi-coding-agent-sessions--group-newest b)))))
    ('recent
     (pi-coding-agent-sessions--newer-p
      (pi-coding-agent-sessions--group-newest a)
      (pi-coding-agent-sessions--group-newest b)))
    (_
     (string-lessp
      (downcase (pi-coding-agent-sessions--project-display-name (car a)))
      (downcase (pi-coding-agent-sessions--project-display-name (car b)))))))

(defun pi-coding-agent-sessions--sort-grouped-records (records)
  "Sort RECORDS into contiguous native project groups."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (record records)
      (push record (gethash (plist-get record :project-root) groups)))
    (mapcan
     (lambda (group)
       (sort group
             (if (eq pi-coding-agent-sessions-sort-order 'recent)
                 #'pi-coding-agent-sessions--newer-p
               #'pi-coding-agent-sessions--attention-less-p)))
     (sort (hash-table-values groups)
           #'pi-coding-agent-sessions--group-less-p))))

(defun pi-coding-agent-sessions--scan-history (&optional include-remote)
  "Return historical records, including remote sources if INCLUDE-REMOTE."
  (delq nil
        (mapcar #'pi-coding-agent-sessions--history-record
                (pi-coding-agent-sessions--history-files include-remote))))

(defun pi-coding-agent-sessions-catalog (&optional refresh-history)
  "Return merged live and historical sessions in grouped project order.
When REFRESH-HISTORY is non-nil, rescan local and observed remote history.
Otherwise reuse cached history, initially scanning only local roots.  This keeps
agent activity and session switching from causing synchronous TRAMP traffic."
  (pi-coding-agent-sessions--ensure-caches)
  (when refresh-history
    (clrhash pi-coding-agent-sessions--project-root-cache)
    (clrhash pi-coding-agent-sessions--working-tree-relation-cache))
  (when (or refresh-history
            (not pi-coding-agent-sessions--history-loaded-p))
    (setq pi-coding-agent-sessions--historical-records
          (pi-coding-agent-sessions--scan-history refresh-history)
          pi-coding-agent-sessions--history-loaded-p t))
  ;; Learn remote roots even when using cached history.  This only examines
  ;; live buffer state and does not contact the remote filesystem.
  (pi-coding-agent-sessions--live-history-directories)
  (let ((records (make-hash-table :test #'equal)))
    (dolist (record pi-coding-agent-sessions--historical-records)
      (when (pi-coding-agent-sessions--record-directory-available-p record)
        (puthash (plist-get record :id) record records)))
    (dolist (chat-buffer (pi-coding-agent-sessions--chat-buffers))
      (let ((record (pi-coding-agent-sessions--live-record chat-buffer)))
        (puthash (plist-get record :id) record records)))
    (pi-coding-agent-sessions--sort-grouped-records
     (pi-coding-agent-sessions--apply-working-tree-groups
      (hash-table-values records)))))

(defun pi-coding-agent-sessions--status-text (status)
  "Return propertized ledger text for STATUS."
  (pcase status
    ('working (propertize "● working" 'face 'pi-coding-agent-sessions-working))
    ('finished (propertize "✓ done" 'face 'pi-coding-agent-sessions-finished))
    ('ready (propertize "○ ready" 'face 'pi-coding-agent-sessions-ready))
    ('history (propertize "· history" 'face 'pi-coding-agent-sessions-history))
    ('stopped (propertize "× stopped" 'face 'pi-coding-agent-sessions-stopped))
    (_ (format "%s" status))))

(defun pi-coding-agent-sessions--relative-time (time)
  "Return concise relative text for TIME."
  (if time (pi-coding-agent--format-relative-time time) "unknown"))

(defun pi-coding-agent-sessions--directory-text (record)
  "Return abbreviated working-directory text for RECORD."
  (pi-coding-agent--truncate-string
   (pi-coding-agent--route-preserving-abbreviate-file-name
    (plist-get record :directory))
   34))

(defun pi-coding-agent-sessions--ledger-entry (record)
  "Return a `tabulated-list-entries' row for RECORD."
  (list
   (plist-get record :id)
   (vector
    (pi-coding-agent-sessions--status-text (plist-get record :status))
    (pi-coding-agent-sessions--directory-text record)
    (pi-coding-agent--truncate-string (plist-get record :label) 60)
    (or (plist-get record :model) "")
    (number-to-string (or (plist-get record :message-count) 0))
    (pi-coding-agent-sessions--relative-time
     (plist-get record :modified-time)))))

(defun pi-coding-agent-sessions--ledger-record-at-point ()
  "Return the ledger session record at point, or nil."
  (and (hash-table-p pi-coding-agent-sessions--records)
       (gethash (tabulated-list-get-id) pi-coding-agent-sessions--records)))

(defun pi-coding-agent-sessions--mark-seen (record)
  "Mark live session RECORD as seen."
  (when-let* ((buffer (plist-get record :buffer)))
    (remhash buffer pi-coding-agent-sessions--finished-buffers)))

(defun pi-coding-agent-sessions--open-record (record)
  "Open or resume session RECORD."
  (unless record
    (user-error "No Pi session at point"))
  (pi-coding-agent-sessions--mark-seen record)
  (cond
   ((buffer-live-p (plist-get record :buffer))
    (let* ((chat-buffer (plist-get record :buffer))
           (input-buffer
            (buffer-local-value 'pi-coding-agent--input-buffer chat-buffer)))
      (pi-coding-agent--show-session-buffers chat-buffer input-buffer)))
   ((plist-get record :path)
    (pi-coding-agent-open-session-file (plist-get record :path)))
   (t
    (user-error "Pi session is no longer available")))
  (pi-coding-agent-sessions--schedule-refresh))

(defun pi-coding-agent-sessions-open ()
  "Open or resume the ledger session at point."
  (interactive)
  (pi-coding-agent-sessions--open-record
   (pi-coding-agent-sessions--ledger-record-at-point)))

(defun pi-coding-agent-sessions--deletable-record-p (record)
  "Return non-nil when RECORD can safely be deleted from the ledger."
  (and (stringp (plist-get record :path))
       (not (buffer-live-p (plist-get record :buffer)))))

(defun pi-coding-agent-sessions-mark-delete ()
  "Mark the historical session at point for deletion."
  (interactive)
  (let ((record (pi-coding-agent-sessions--ledger-record-at-point)))
    (unless (pi-coding-agent-sessions--deletable-record-p record)
      (user-error "Only non-live Pi sessions can be deleted"))
    (puthash (plist-get record :id) t pi-coding-agent-sessions--delete-marks)
    (tabulated-list-put-tag "D" t)))

(defun pi-coding-agent-sessions-unmark ()
  "Remove the deletion mark from the session at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (remhash id pi-coding-agent-sessions--delete-marks)
    (tabulated-list-put-tag " " t)))

(defun pi-coding-agent-sessions-delete-marked ()
  "Delete session files marked in the Agent Ledger."
  (interactive)
  (let ((records
         (delq nil
               (mapcar
                (lambda (id)
                  (gethash id pi-coding-agent-sessions--records))
                (hash-table-keys pi-coding-agent-sessions--delete-marks)))))
    (unless records
      (user-error "No Pi sessions marked for deletion"))
    (when (yes-or-no-p
           (format "Delete %d Pi session file%s? "
                   (length records) (if (= (length records) 1) "" "s")))
      (let ((deleted 0)
            (failed nil))
        (dolist (record records)
          (let ((id (plist-get record :id))
                (path (plist-get record :path)))
            (condition-case error-data
                (progn
                  (when (file-exists-p path)
                    (delete-file path))
                  (remhash path pi-coding-agent-sessions--metadata-cache)
                  (setq pi-coding-agent-sessions--historical-records
                        (cl-delete id pi-coding-agent-sessions--historical-records
                                   :key (lambda (item) (plist-get item :id))
                                   :test #'equal))
                  (remhash id pi-coding-agent-sessions--delete-marks)
                  (setq deleted (1+ deleted)))
              (error
               (push (format "%s: %s" path (error-message-string error-data))
                     failed)))))
        (pi-coding-agent-sessions-refresh)
        (if failed
            (message "Pi: deleted %d session%s; failed: %s"
                     deleted (if (= deleted 1) "" "s")
                     (string-join (nreverse failed) "; "))
          (message "Pi: deleted %d session%s"
                   deleted (if (= deleted 1) "" "s")))))))

(defun pi-coding-agent-sessions-new ()
  "Start or focus a Pi session for the project at point."
  (interactive)
  (let* ((record (pi-coding-agent-sessions--ledger-record-at-point))
         (root (and record (plist-get record :project-root))))
    (unless root
      (user-error "No Pi project at point"))
    (let ((default-directory root))
      (call-interactively #'pi-coding-agent))))

(defun pi-coding-agent-sessions--completion-string (record)
  "Return propertized completion display text for RECORD."
  (format "%-10s %-24s %s · %s"
          (pi-coding-agent-sessions--status-text (plist-get record :status))
          (pi-coding-agent--truncate-string
           (pi-coding-agent--route-preserving-abbreviate-file-name
            (plist-get record :directory))
           24)
          (pi-coding-agent--truncate-string (plist-get record :label) 60)
          (pi-coding-agent-sessions--relative-time
           (plist-get record :modified-time))))

;;;###autoload
(defun pi-coding-agent-switch-session ()
  "Switch to a live or historical Pi session across projects."
  (interactive)
  (let* ((records (pi-coding-agent-sessions-catalog))
         (indexed (cl-loop for record in records
                           for index from 1
                           collect (cons
                                    (format "%s  %s"
                                            (pi-coding-agent-sessions--completion-string record)
                                            (propertize (format "[%d]" index) 'face 'shadow))
                                    record))))
    (if (null indexed)
        (message "Pi: No sessions found")
      (let* ((candidate-records (make-hash-table :test #'equal))
             (_ (dolist (candidate indexed)
                  (puthash (car candidate) (cdr candidate) candidate-records)))
             (candidate-strings (mapcar #'car indexed))
             (group-function
              (lambda (candidate transform)
                (if transform
                    candidate
                  (pi-coding-agent-sessions--project-display-name
                   (gethash candidate candidate-records)))))
             (collection
              (lambda (string predicate action)
                (if (eq action 'metadata)
                    `(metadata
                      (category . pi-coding-agent-session)
                      (display-sort-function . identity)
                      (cycle-sort-function . identity)
                      (group-function . ,group-function))
                  (complete-with-action action candidate-strings
                                        string predicate))))
             (choice (completing-read "Pi session: " collection nil t))
             (record (cdr (assoc choice indexed))))
        (when record
          (pi-coding-agent-sessions--open-record record))))))

(defun pi-coding-agent-sessions--goto-id (id)
  "Move point to ledger row ID and return non-nil when found."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (equal (tabulated-list-get-id) id)
          (setq found t)
        (forward-line 1)))
    found))

(defun pi-coding-agent-sessions--insert-project-heading (record first-p)
  "Insert a project heading for RECORD.
FIRST-P is non-nil for the first heading in the ledger."
  (unless first-p
    (insert "\n"))
  (insert
   (propertize
    (format "Project: %s  %s\n"
            (pi-coding-agent-sessions--project-display-name record)
            (pi-coding-agent--route-preserving-abbreviate-file-name
             (plist-get record :project-root)))
    'face 'pi-coding-agent-sessions-project-heading)))

(defun pi-coding-agent-sessions--print-grouped (remember-id)
  "Print grouped ledger rows and restore point to REMEMBER-ID."
  (let ((inhibit-read-only t)
        (project-root nil)
        (first-heading t))
    (erase-buffer)
    (dolist (entry tabulated-list-entries)
      (let* ((id (car entry))
             (record (gethash id pi-coding-agent-sessions--records))
             (record-root (plist-get record :project-root)))
        (unless (equal record-root project-root)
          (setq project-root record-root)
          (pi-coding-agent-sessions--insert-project-heading
           record first-heading)
          (setq first-heading nil))
        (let ((row-start (point)))
          (tabulated-list-print-entry id (cadr entry))
          (when (gethash id pi-coding-agent-sessions--delete-marks)
            (save-excursion
              (goto-char row-start)
              (tabulated-list-put-tag "D"))))))
    (goto-char (point-min))
    (when remember-id
      (pi-coding-agent-sessions--goto-id remember-id))))

(defun pi-coding-agent-sessions-refresh (&optional refresh-history)
  "Refresh the Pi Agent Ledger.
Interactively, rescan local and observed remote history.  Internal activity
refreshes omit REFRESH-HISTORY and update live rows from cached history only."
  (interactive (list t))
  (let ((id (tabulated-list-get-id))
        (records (pi-coding-agent-sessions-catalog refresh-history)))
    (setq pi-coding-agent-sessions--records (make-hash-table :test #'equal))
    (dolist (record records)
      (puthash (plist-get record :id) record
               pi-coding-agent-sessions--records))
    (dolist (marked-id (hash-table-keys pi-coding-agent-sessions--delete-marks))
      (unless (gethash marked-id pi-coding-agent-sessions--records)
        (remhash marked-id pi-coding-agent-sessions--delete-marks)))
    (setq tabulated-list-entries
          (mapcar #'pi-coding-agent-sessions--ledger-entry records))
    (pi-coding-agent-sessions--print-grouped id)))

(defun pi-coding-agent-sessions--revert (_ignore-auto _noconfirm)
  "Revert the ledger by rebuilding its session catalog."
  (pi-coding-agent-sessions-refresh))

(defun pi-coding-agent-sessions-cycle-sort ()
  "Cycle project, attention, and recent ordering for session UIs."
  (interactive)
  (setq pi-coding-agent-sessions-sort-order
        (pcase pi-coding-agent-sessions-sort-order
          ('project 'attention)
          ('attention 'recent)
          (_ 'project)))
  (when (derived-mode-p 'pi-coding-agent-sessions-mode)
    (setq tabulated-list-sort-key nil)
    (pi-coding-agent-sessions-refresh))
  (message "Pi sessions sorted by %s" pi-coding-agent-sessions-sort-order))

(defvar-keymap pi-coding-agent-sessions-mode-map
  :parent tabulated-list-mode-map
  "RET" #'pi-coding-agent-sessions-open
  "o" #'pi-coding-agent-sessions-open
  "n" #'pi-coding-agent-sessions-new
  "s" #'pi-coding-agent-switch-session
  "S" #'pi-coding-agent-sessions-cycle-sort
  "d" #'pi-coding-agent-sessions-mark-delete
  "u" #'pi-coding-agent-sessions-unmark
  "x" #'pi-coding-agent-sessions-delete-marked
  "g" #'pi-coding-agent-sessions-refresh
  "q" #'quit-window)

(define-derived-mode pi-coding-agent-sessions-mode tabulated-list-mode
  "Pi-Sessions"
  "Major mode for browsing live and historical Pi sessions."
  (setq tabulated-list-format
        [("State" 11 nil)
         ("Directory / worktree" 34 nil)
         ("Session" 60 nil)
         ("Model" 22 nil)
         ("Msgs" 6 nil :right-align t)
         ("Activity" 12 nil)])
  (setq tabulated-list-padding 2
        tabulated-list-sort-key nil
        revert-buffer-function #'pi-coding-agent-sessions--revert
        pi-coding-agent-sessions--delete-marks (make-hash-table :test #'equal)
        header-line-format
        '(:eval (pi-coding-agent-sessions--ledger-header-string)))
  (tabulated-list-init-header))

(defun pi-coding-agent-sessions--ledger-header-string ()
  "Return the ledger header summary."
  (let ((active 0)
        (finished 0)
        (projects (make-hash-table :test #'equal)))
    (when (hash-table-p pi-coding-agent-sessions--records)
      (maphash
       (lambda (_id record)
         (puthash (plist-get record :project-root) t projects)
         (pcase (plist-get record :status)
           ('working (setq active (1+ active)))
           ('finished (setq finished (1+ finished)))))
       pi-coding-agent-sessions--records))
    (format " Pi Agent Ledger  |  %d projects  |  %d active  |  %d completed  |  sort: %s"
            (hash-table-count projects) active finished
            pi-coding-agent-sessions-sort-order)))

;;;###autoload
(defun pi-coding-agent-sessions ()
  "Display the cross-project Pi Agent Ledger."
  (interactive)
  (let ((buffer (get-buffer-create pi-coding-agent-sessions-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'pi-coding-agent-sessions-mode)
        (pi-coding-agent-sessions-mode))
      (pi-coding-agent-sessions-refresh))
    (pop-to-buffer buffer)))

(defun pi-coding-agent-sessions--ledger-visible-p ()
  "Return non-nil when the ledger is visible in some frame."
  (get-buffer-window pi-coding-agent-sessions-buffer-name t))

(defun pi-coding-agent-sessions--run-refresh ()
  "Refresh visible session UI after a debounced state change."
  (setq pi-coding-agent-sessions--refresh-timer nil)
  (when-let* ((buffer (get-buffer pi-coding-agent-sessions-buffer-name))
              ((pi-coding-agent-sessions--ledger-visible-p)))
    (with-current-buffer buffer
      (pi-coding-agent-sessions-refresh)))
  (force-mode-line-update t))

(defun pi-coding-agent-sessions--schedule-refresh (&rest _ignored)
  "Schedule a debounced refresh of visible session UI."
  (force-mode-line-update t)
  (when (pi-coding-agent-sessions--ledger-visible-p)
    (when pi-coding-agent-sessions--refresh-timer
      (ignore-errors
        (cancel-timer pi-coding-agent-sessions--refresh-timer)))
    (setq pi-coding-agent-sessions--refresh-timer
          (run-with-idle-timer 0.25 nil
                              #'pi-coding-agent-sessions--run-refresh))))

(defun pi-coding-agent-sessions--track-activity
    (chat-buffer _input-buffer old-phase new-phase reason)
  "Track completion transitions for CHAT-BUFFER.
OLD-PHASE and NEW-PHASE are activity phase strings.  REASON distinguishes
real phase changes from buffer lifecycle synchronization."
  (cond
   ((and (eq reason 'phase-change)
         (not (equal old-phase "idle"))
         (equal new-phase "idle"))
    (puthash chat-buffer t pi-coding-agent-sessions--finished-buffers))
   ((not (equal new-phase "idle"))
    (remhash chat-buffer pi-coding-agent-sessions--finished-buffers)))
  (pi-coding-agent-sessions--schedule-refresh))

(defun pi-coding-agent-sessions--live-counts ()
  "Return cons of active and completed-unseen live session counts."
  (let ((active 0)
        (finished 0))
    (dolist (buffer (pi-coding-agent-sessions--chat-buffers))
      (pcase (pi-coding-agent-sessions--live-status buffer)
        ('working (setq active (1+ active)))
        ('finished (setq finished (1+ finished)))))
    (cons active finished)))

(defvar pi-coding-agent-sessions--header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'pi-coding-agent-sessions)
    map)
  "Mouse map for the global session overview in Pi headers.")

(defun pi-coding-agent-session-indicator-string ()
  "Return a clickable global Pi session indicator for the header line."
  (when pi-coding-agent-sessions-show-header-overview
    (pcase-let ((`(,active . ,finished)
                 (pi-coding-agent-sessions--live-counts)))
      (when (or (> active 0) (> finished 0))
        (concat
         " │ "
         (propertize
          (string-join
           (delq nil
                 (list (and (> active 0) (format "%d active" active))
                       (and (> finished 0) (format "%d done" finished))))
           " · ")
          'face (if (> finished 0) 'success 'warning)
          'mouse-face 'highlight
          'help-echo "mouse-1: Open Pi Agent Ledger"
          'local-map pi-coding-agent-sessions--header-map))))))

(add-hook 'pi-coding-agent-activity-phase-functions
          #'pi-coding-agent-sessions--track-activity)

(provide 'pi-coding-agent-sessions)
;;; pi-coding-agent-sessions.el ends here
