;;; pi-coding-agent-sessions-test.el --- Session browser tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pi-coding-agent-test-common)
(require 'pi-coding-agent-sessions)

(ert-deftest pi-coding-agent-test-sessions-history-record-uses-metadata ()
  "Historical records expose project, label, count, and path metadata."
  (let* ((project (pi-coding-agent-test--make-temp-directory
                   "pi-coding-agent-ledger-project-"))
         (sessions (pi-coding-agent-test--make-temp-directory
                    "pi-coding-agent-ledger-sessions-"))
         (path (expand-file-name "session.jsonl" sessions)))
    (unwind-protect
        (progn
          (pi-coding-agent-test--write-session-file path "First request" project)
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-max))
            (insert (json-serialize
                     '(:type "session_info" :name "Named work"))
                    "\n")
            (write-region nil nil path nil 'silent))
          (clrhash pi-coding-agent-sessions--metadata-cache)
          (let ((record (pi-coding-agent-sessions--history-record path)))
            (should (equal (plist-get record :path) path))
            (should (equal (plist-get record :project-root) project))
            (should (equal (plist-get record :label) "Named work"))
            (should (eq (plist-get record :status) 'history))))
      (delete-directory project t)
      (delete-directory sessions t))))

(ert-deftest pi-coding-agent-test-sessions-catalog-live-record-replaces-history ()
  "A live session replaces the historical row for the same session file."
  (let* ((pi-coding-agent-sessions--history-loaded-p nil)
         (pi-coding-agent-sessions--historical-records nil)
         (path "/tmp/pi-ledger-session.jsonl")
         (history (list :id (concat "file:" path)
                        :path path :status 'history
                        :project-root "/tmp/project/"
                        :project-name "project"
                        :modified-time '(1 0 0 0)))
         (live (list :id (concat "file:" path)
                     :path path :status 'working
                     :project-root "/tmp/project/"
                     :project-name "project"
                     :modified-time '(2 0 0 0))))
    (cl-letf (((symbol-function 'pi-coding-agent-sessions--history-files)
               (lambda (&optional _include-remote) (list path)))
              ((symbol-function 'pi-coding-agent-sessions--history-record)
               (lambda (_path) history))
              ((symbol-function 'pi-coding-agent-sessions--chat-buffers)
               (lambda () '(mock-chat)))
              ((symbol-function 'pi-coding-agent-sessions--live-record)
               (lambda (_buffer) live)))
      (should (equal (pi-coding-agent-sessions-catalog) (list live))))))

(ert-deftest pi-coding-agent-test-sessions-catalog-orders-attention-first ()
  "Working and completed sessions sort before ready and historical rows."
  (let ((pi-coding-agent-sessions-sort-order 'attention)
        (records
         (list (list :id "history" :status 'history :modified-time '(4 0 0 0))
               (list :id "ready" :status 'ready :modified-time '(3 0 0 0))
               (list :id "done" :status 'finished :modified-time '(2 0 0 0))
               (list :id "work" :status 'working :modified-time '(1 0 0 0)))))
    (should
     (equal (mapcar (lambda (record) (plist-get record :id))
                    (sort records #'pi-coding-agent-sessions--record-less-p))
            '("work" "done" "ready" "history")))))

(ert-deftest pi-coding-agent-test-sessions-open-live-record-shows-linked-buffers ()
  "Opening a live row displays its chat and input buffers."
  (let ((chat (generate-new-buffer " *pi-ledger-chat*"))
        (input (generate-new-buffer " *pi-ledger-input*"))
        (shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local pi-coding-agent--input-buffer input))
          (puthash chat t pi-coding-agent-sessions--finished-buffers)
          (cl-letf (((symbol-function 'pi-coding-agent--show-session-buffers)
                     (lambda (shown-chat shown-input)
                       (setq shown (list shown-chat shown-input))))
                    ((symbol-function 'pi-coding-agent-sessions--schedule-refresh)
                     #'ignore))
            (pi-coding-agent-sessions--open-record
             (list :buffer chat :status 'finished)))
          (should (equal shown (list chat input)))
          (should-not (gethash chat pi-coding-agent-sessions--finished-buffers)))
      (kill-buffer input)
      (kill-buffer chat))))

(ert-deftest pi-coding-agent-test-sessions-open-history-uses-public-opener ()
  "Opening a historical row delegates to the validated session-file opener."
  (let ((opened nil))
    (cl-letf (((symbol-function 'pi-coding-agent-open-session-file)
               (lambda (path) (setq opened path)))
              ((symbol-function 'pi-coding-agent-sessions--schedule-refresh)
               #'ignore))
      (pi-coding-agent-sessions--open-record
       '(:path "/tmp/history.jsonl" :status history))
      (should (equal opened "/tmp/history.jsonl")))))

(ert-deftest pi-coding-agent-test-sessions-track-completed-unseen ()
  "Busy-to-idle transitions mark a session completed until reopened."
  (let ((chat (generate-new-buffer " *pi-ledger-finished*")))
    (unwind-protect
        (cl-letf (((symbol-function 'pi-coding-agent-sessions--schedule-refresh)
                   #'ignore))
          (pi-coding-agent-sessions--track-activity
           chat nil "replying" "idle" 'phase-change)
          (should (gethash chat pi-coding-agent-sessions--finished-buffers))
          (pi-coding-agent-sessions--track-activity
           chat nil "idle" "thinking" 'phase-change)
          (should-not (gethash chat pi-coding-agent-sessions--finished-buffers)))
      (remhash chat pi-coding-agent-sessions--finished-buffers)
      (kill-buffer chat))))

(ert-deftest pi-coding-agent-test-sessions-ledger-refresh-preserves-row-id ()
  "Ledger refresh builds rows keyed by stable session ids."
  (let ((record '(:id "session-1"
                  :status history
                  :project-name "demo"
                  :project-root "/tmp/demo/"
                  :directory "/tmp/demo-worktree/"
                  :label "Investigate issue"
                  :model ""
                  :message-count 4
                  :modified-time (1 0 0 0))))
    (with-temp-buffer
      (pi-coding-agent-sessions-mode)
      (cl-letf (((symbol-function 'pi-coding-agent-sessions-catalog)
                 (lambda (&optional _refresh-history) (list record))))
        (pi-coding-agent-sessions-refresh))
      (should (equal (caar tabulated-list-entries) "session-1"))
      (should (equal (gethash "session-1" pi-coding-agent-sessions--records)
                     record))
      (should (string-match-p "Project: demo" (buffer-string)))
      (should (string-match-p "/tmp/demo-worktree/" (buffer-string))))))

(ert-deftest pi-coding-agent-test-sessions-switcher-opens-selected-record ()
  "The cross-project switcher opens the selected completion record."
  (let* ((record '(:id "selected" :status ready
                   :project-name "demo" :directory "/tmp/demo-worktree/"
                   :label "Chosen work"
                   :modified-time (1 0 0 0)))
         (opened nil))
    (cl-letf (((symbol-function 'pi-coding-agent-sessions-catalog)
               (lambda () (list record)))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (let* ((candidate (car (all-completions "" collection)))
                        (metadata (completion-metadata "" collection nil))
                        (group-function
                         (completion-metadata-get metadata 'group-function)))
                   (should (eq (completion-metadata-get
                                metadata 'display-sort-function)
                               #'identity))
                   (should (equal (funcall group-function candidate nil)
                                  "demo"))
                   candidate)))
              ((symbol-function 'pi-coding-agent-sessions--open-record)
               (lambda (chosen) (setq opened chosen))))
      (pi-coding-agent-switch-session)
      (should (equal opened record)))))

(ert-deftest pi-coding-agent-test-sessions-header-indicator-shows-live-counts ()
  "The header overview reports active and newly completed sessions."
  (let ((pi-coding-agent-sessions-show-header-overview t))
    (cl-letf (((symbol-function 'pi-coding-agent-sessions--live-counts)
               (lambda () '(2 . 1))))
      (let ((indicator (pi-coding-agent-session-indicator-string)))
        (should (string-match-p "2 active" indicator))
        (should (string-match-p "1 done" indicator))
        (should (keymapp (get-text-property
                          (1- (length indicator)) 'local-map indicator)))))))

(ert-deftest pi-coding-agent-test-sessions-mode-binds-navigation-commands ()
  "The ledger exposes native open, refresh, new, and switch commands."
  (should (eq (lookup-key pi-coding-agent-sessions-mode-map (kbd "RET"))
              #'pi-coding-agent-sessions-open))
  (should (eq (lookup-key pi-coding-agent-sessions-mode-map (kbd "g"))
              #'pi-coding-agent-sessions-refresh))
  (should (eq (lookup-key pi-coding-agent-sessions-mode-map (kbd "n"))
              #'pi-coding-agent-sessions-new))
  (should (eq (lookup-key pi-coding-agent-sessions-mode-map (kbd "s"))
              #'pi-coding-agent-switch-session))
  (should (eq (lookup-key pi-coding-agent-sessions-mode-map (kbd "S"))
              #'pi-coding-agent-sessions-cycle-sort))
  (should (eq (lookup-key pi-coding-agent-sessions-mode-map (kbd "d"))
              #'pi-coding-agent-sessions-mark-delete))
  (should (eq (lookup-key pi-coding-agent-sessions-mode-map (kbd "u"))
              #'pi-coding-agent-sessions-unmark))
  (should (eq (lookup-key pi-coding-agent-sessions-mode-map (kbd "x"))
              #'pi-coding-agent-sessions-delete-marked)))

(ert-deftest pi-coding-agent-test-sessions-label-collapses-newlines ()
  "Session labels cannot split or interleave tabulated rows."
  (should
   (equal (pi-coding-agent-sessions--session-label
           '(:first-message "First line\n\tSecond   line") nil)
          "First line Second line")))

(ert-deftest pi-coding-agent-test-sessions-native-project-root-canonicalizes-directory ()
  "Catalog grouping asks project.el for the canonical project root."
  (cl-letf (((symbol-function 'project-current)
             (lambda (_prompt directory)
               (should (equal directory "/tmp/project/subdir/"))
               'mock-project))
            ((symbol-function 'project-root)
             (lambda (project)
               (should (eq project 'mock-project))
               "/tmp/project/")))
    (should (equal (pi-coding-agent-sessions--native-project-root
                    "/tmp/project/subdir/")
                   "/tmp/project/"))))

(ert-deftest pi-coding-agent-test-sessions-queries-generic-vc-working-trees ()
  "Working-tree discovery dispatches through the generic VC operation."
  (let ((pi-coding-agent-sessions--working-tree-relation-cache
         (make-hash-table :test #'equal))
        (root "/tmp/coreboot/")
        (other "/tmp/coreboot_Atombios/"))
    (cl-letf (((symbol-function 'vc-responsible-backend)
               (lambda (directory)
                 (should (equal directory root))
                 'JJ))
              ((symbol-function 'vc-find-backend-function)
               (lambda (backend operation)
                 (should (eq backend 'JJ))
                 (should (eq operation 'known-other-working-trees))
                 (lambda () (list other)))))
      (should
       (equal (pi-coding-agent-sessions--known-working-tree-roots root)
              (list root other))))))

(ert-deftest pi-coding-agent-test-sessions-invalidates-vc-cache-after-backend-load ()
  "Loading a previously missing VC operation invalidates singleton results."
  (let ((pi-coding-agent-sessions--working-tree-relation-cache
         (make-hash-table :test #'equal))
        (root "/tmp/coreboot/")
        (other "/tmp/coreboot-worktree/")
        (operation nil))
    ;; Reproduce a catalog lookup before vc-jj's operation has been loaded.
    (puthash root (list :function nil :roots (list root))
             pi-coding-agent-sessions--working-tree-relation-cache)
    (cl-letf (((symbol-function 'vc-responsible-backend) (lambda (_) 'JJ))
              ((symbol-function 'vc-find-backend-function)
               (lambda (_backend _operation) operation)))
      (should (equal (pi-coding-agent-sessions--known-working-tree-roots root)
                     (list root)))
      (setq operation (lambda () (list other)))
      (should (equal (pi-coding-agent-sessions--known-working-tree-roots root)
                     (list root other))))))

(ert-deftest pi-coding-agent-test-sessions-anchors-remote-vc-working-trees ()
  "Process-local VC roots inherit the current project's TRAMP route."
  (let ((pi-coding-agent-sessions--working-tree-relation-cache
         (make-hash-table :test #'equal))
        (root "/ssh:pi-host:/srv/coreboot/"))
    (cl-letf (((symbol-function 'vc-responsible-backend) (lambda (_) 'Git))
              ((symbol-function 'vc-find-backend-function)
               (lambda (_backend _operation)
                 (lambda () '("/srv/coreboot-worktree/")))))
      (should
       (equal (pi-coding-agent-sessions--known-working-tree-roots root)
              '("/ssh:pi-host:/srv/coreboot/"
                "/ssh:pi-host:/srv/coreboot-worktree/"))))))

(ert-deftest pi-coding-agent-test-sessions-project-label-includes-tramp-route ()
  "Remote projects are distinguishable in ledger and completion groups."
  (should
   (equal
    (pi-coding-agent-sessions--project-display-name
     '(:project-name "coreboot"
       :project-root "/ssh:bastion|sudo:root@pi-host:/srv/coreboot/"))
    "coreboot @ ssh:bastion|sudo:root@pi-host")))

(ert-deftest pi-coding-agent-test-sessions-catalog-reinitializes-hot-reload-caches ()
  "Catalog refresh repairs cache variables left nil by an older loaded version."
  (let ((pi-coding-agent-sessions--project-root-cache nil)
        (pi-coding-agent-sessions--working-tree-relation-cache nil)
        (pi-coding-agent-sessions--observed-remote-history-directories nil)
        (pi-coding-agent-sessions-history-roots nil)
        (pi-coding-agent-sessions--history-loaded-p t)
        (pi-coding-agent-sessions--historical-records nil))
    (cl-letf (((symbol-function 'pi-coding-agent-sessions--chat-buffers)
               (lambda () nil)))
      (should-not (pi-coding-agent-sessions-catalog t))
      (should (hash-table-p pi-coding-agent-sessions--project-root-cache))
      (should (hash-table-p
               pi-coding-agent-sessions--working-tree-relation-cache))
      (should (hash-table-p
               pi-coding-agent-sessions--observed-remote-history-directories)))))

(ert-deftest pi-coding-agent-test-sessions-passive-history-scan-skips-remote ()
  "Initial history loading does not contact configured TRAMP roots."
  (let ((pi-coding-agent-sessions-history-roots
         '("/tmp/local-sessions/" "/ssh:pi-host:~/.pi/agent/sessions/"))
        (seen nil))
    (cl-letf (((symbol-function 'pi-coding-agent-sessions--root-directories)
               (lambda (root)
                 (push root seen)
                 nil))
              ((symbol-function 'pi-coding-agent-sessions--live-history-directories)
               (lambda () nil)))
      (pi-coding-agent-sessions--history-files)
      (should (equal seen '("/tmp/local-sessions/"))))))

(ert-deftest pi-coding-agent-test-sessions-explicit-history-scan-includes-observed-remote ()
  "Explicit history refresh includes remote directories learned from live Pi."
  (let ((pi-coding-agent-sessions-history-roots nil)
        (pi-coding-agent-sessions--observed-remote-history-directories
         (make-hash-table :test #'equal))
        (remote "/ssh:pi-host:/home/pi/.pi/agent/sessions/project/")
        (seen nil))
    (puthash remote t pi-coding-agent-sessions--observed-remote-history-directories)
    (cl-letf (((symbol-function 'pi-coding-agent-sessions--live-history-directories)
               (lambda () nil))
              ((symbol-function 'pi-coding-agent-sessions--recent-files)
               (lambda (directory)
                 (push directory seen)
                 nil)))
      (pi-coding-agent-sessions--history-files t)
      (should (equal seen (list remote))))))

(ert-deftest pi-coding-agent-test-sessions-groups-connected-working-trees ()
  "Sessions use connected VC working trees as one project group."
  (let* ((primary "/tmp/coreboot/")
         (worktree "/tmp/coreboot_Atombios/")
         (primary-record
          (list :id "primary" :project-root primary
                :project-name "coreboot"))
         (worktree-record
          (list :id "worktree" :project-root worktree
                :project-name "coreboot_Atombios"))
         (records (list primary-record worktree-record)))
    (cl-letf (((symbol-function
                'pi-coding-agent-sessions--known-working-tree-roots)
               (lambda (root)
                 ;; Older JJ workspaces may omit the primary path when queried
                 ;; from a linked workspace.  The reverse relation still joins
                 ;; the connected component.
                 (if (equal root primary)
                     (list primary worktree)
                   (list worktree)))))
      (pi-coding-agent-sessions--apply-working-tree-groups records)
      (dolist (record records)
        (should (equal (plist-get record :project-root) primary))
        (should (equal (plist-get record :project-name) "coreboot"))))))

(ert-deftest pi-coding-agent-test-sessions-project-sort-groups-projects ()
  "Default ordering groups projects before considering session status."
  (let ((pi-coding-agent-sessions-sort-order 'project)
        (records
         (list '(:id "z-working" :project-name "zeta"
                 :project-root "/zeta/" :status working
                 :modified-time (4 0 0 0))
               '(:id "a-history" :project-name "alpha"
                 :project-root "/alpha/" :status history
                 :modified-time (1 0 0 0))
               '(:id "a-working" :project-name "alpha"
                 :project-root "/alpha/" :status working
                 :modified-time (2 0 0 0)))))
    (should
     (equal (mapcar (lambda (record) (plist-get record :id))
                    (pi-coding-agent-sessions--sort-grouped-records records))
            '("a-working" "a-history" "z-working")))))

(ert-deftest pi-coding-agent-test-sessions-catalog-hides-deleted-worktree ()
  "Historical sessions disappear when their local working tree is gone."
  (let* ((pi-coding-agent-sessions-hide-missing-directories t)
         (pi-coding-agent-sessions--history-loaded-p t)
         (pi-coding-agent-sessions--historical-records
          (list '(:id "stale" :directory "/path/that/no-longer/exists/"
                  :project-root "/path/that/no-longer/exists/")))
         (pi-coding-agent-sessions--project-root-cache
          (make-hash-table :test #'equal))
         (pi-coding-agent-sessions--working-tree-relation-cache
          (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'pi-coding-agent-sessions--chat-buffers)
               (lambda () nil))
              ((symbol-function 'pi-coding-agent-sessions--live-history-directories)
               (lambda () nil)))
      (should-not (pi-coding-agent-sessions-catalog)))))

(ert-deftest pi-coding-agent-test-sessions-mark-delete-tags-row ()
  "Marking a historical session records and displays its deletion tag."
  (let ((record '(:id "session-1" :path "/tmp/session-1.jsonl"
                  :status history :project-name "demo"
                  :project-root "/tmp/demo/" :directory "/tmp/demo/"
                  :label "Old work" :message-count 1)))
    (with-temp-buffer
      (pi-coding-agent-sessions-mode)
      (cl-letf (((symbol-function 'pi-coding-agent-sessions-catalog)
                 (lambda (&optional _) (list record))))
        (pi-coding-agent-sessions-refresh))
      (should (pi-coding-agent-sessions--goto-id "session-1"))
      (pi-coding-agent-sessions-mark-delete)
      (should (gethash "session-1" pi-coding-agent-sessions--delete-marks))
      (forward-line -1)
      (should (looking-at-p "D")))))

(ert-deftest pi-coding-agent-test-sessions-delete-marked-removes-file ()
  "Executing ledger deletion removes marked historical session files."
  (let* ((path (make-temp-file "pi-ledger-delete-" nil ".jsonl"))
         (id (concat "file:" path))
         (record (list :id id :path path :status 'history))
         (pi-coding-agent-sessions--historical-records (list record))
         (pi-coding-agent-sessions--metadata-cache
          (make-hash-table :test #'equal)))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-sessions-mode)
          (setq pi-coding-agent-sessions--records (make-hash-table :test #'equal))
          (puthash id record pi-coding-agent-sessions--records)
          (puthash id t pi-coding-agent-sessions--delete-marks)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'pi-coding-agent-sessions-refresh) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (pi-coding-agent-sessions-delete-marked))
          (should-not (file-exists-p path))
          (should-not pi-coding-agent-sessions--historical-records)
          (should-not (gethash id pi-coding-agent-sessions--delete-marks)))
      (when (file-exists-p path)
        (delete-file path)))))

(provide 'pi-coding-agent-sessions-test)
;;; pi-coding-agent-sessions-test.el ends here
