;;; test-helpers.el --- Helper utilities for git-spice integration tests -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jesse Claven

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Helper macros and utilities for integration testing git-spice.el
;; with actual gs command execution in temporary git repositories.

;;; Code:

(require 'ert)
(eval-when-compile (require 'cl-macs))

(defmacro gs-integration-skip-unless ()
  "Skip test if gs is not available.
Use this at the start of integration tests that require the gs binary."
  `(unless (executable-find "gs")
     (ert-skip "gs binary not found - skipping integration test")))

(defmacro with-temp-git-repo (&rest body)
  "Create a temporary git repository.
Cleans up the directory after BODY completes.
Sets up git config with test user identity."
  (declare (indent 0) (debug (body)))
  `(let* ((--gs-temp-repo-dir-- (make-temp-file "gs-test-repo-" t))
          (default-directory --gs-temp-repo-dir--)
          ;; Set git environment for isolated testing
          (process-environment
           (append
            '("GIT_AUTHOR_NAME=Test User"
              "GIT_AUTHOR_EMAIL=test@example.com"
              "GIT_COMMITTER_NAME=Test User"
              "GIT_COMMITTER_EMAIL=test@example.com"
              "GIT_CONFIG_GLOBAL=/dev/null"
              "GIT_CONFIG_NOSYSTEM=1")
            process-environment)))
     (unwind-protect
         (progn
           ;; Initialise git repo with main branch
           (call-process "git" nil nil nil "init" "-b" "main")
           (call-process "git" nil nil nil "config" "user.name" "Test User")
           (call-process "git" nil nil nil "config" "user.email" "test@example.com")
           ,@body)
       ;; Cleanup: delete temp directory
       (delete-directory --gs-temp-repo-dir-- t))))

(defmacro with-gs-initialised (&rest body)
  "Run BODY in a git-spice initialised repository.
Creates an initial commit and runs `gs repo init`.
Must be used inside `with-temp-git-repo'."
  (declare (indent 0) (debug (body)))
  `(let ((default-directory default-directory))
     ;; Create initial commit to have a valid git history
     (write-region "initial" nil "initial.txt" nil 'silent)
     (call-process "git" nil nil nil "add" "initial.txt")
     (call-process "git" nil nil nil "commit" "-m" "Initial commit")
     ;; Initialise git-spice
     (call-process "gs" nil nil nil "repo" "init")
     ,@body))

(defun gs-integration-branch-exists-p (branch-name)
  "Check if BRANCH-NAME exists in the current git repository.
Returns non-nil if the branch exists, nil otherwise."
  (let ((output (string-trim
                 (shell-command-to-string
                  (format "git branch --list %s" branch-name)))))
    (not (string-empty-p output))))

(defun gs-integration-get-current-branch ()
  "Return the name of the current git branch.
Returns nil if not in a git repository or on detached HEAD."
  (string-trim
   (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null || echo ''")))

(defun gs-integration-wait-for-gs-process (timeout-secs)
  "Wait up to TIMEOUT-SECS for the *gs* buffer process to finish.
Uses accept-process-output to allow Emacs to process async events.
Returns t if process finished, nil if timed out."
  (let ((start-time (current-time)))
    ;; Wait for process to be created
    (while (and (not (get-buffer-process "*gs*"))
                (< (float-time (time-since start-time)) 1))
      (accept-process-output nil 0.01))
    ;; Wait for process to finish
    (while (let ((proc (get-buffer-process "*gs*")))
             (and proc
                  (process-live-p proc)
                  (< (float-time (time-since start-time)) timeout-secs)))
      (accept-process-output nil 0.1))
    ;; Return t if process is done (doesn't exist or is not live)
    (let ((proc (get-buffer-process "*gs*")))
      (or (not proc)
          (not (process-live-p proc))))))

(defun gs-integration-cleanup-gs-buffer ()
  "Clean up the *gs* buffer and any associated process."
  (when (get-buffer "*gs*")
    (let ((proc (get-buffer-process "*gs*")))
      (when proc
        (while (process-live-p proc)
          (accept-process-output proc 0.1))
        (delete-process proc)))
    (kill-buffer "*gs*")))

(provide 'test-helpers)
;;; test-helpers.el ends here
