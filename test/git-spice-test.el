;;; git-spice-test.el --- Tests for git-spice -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jesse Claven

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Tests for git-spice.el package functionality.

;;; Code:

(require 'ert)

;; Explicitly load git-spice (eldev ensures dependencies are available)
(eval-and-compile
  (require 'git-spice))

;;; Helper Function Tests (require git-spice to be loaded)

(ert-deftest git-spice-test-arguments-default ()
  "Test git-spice-arguments with default prompt."
  (let ((transient-current-prefix 'git-spice-menu)
        (transient-current-command 'git-spice-menu))
    ;; Mock transient-args to return empty list
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '())))
      (should (equal (git-spice-arguments) '())))))

(ert-deftest git-spice-test-arguments-with-prompt ()
  "Test git-spice-arguments with custom prompt."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (prompt)
               (cond
                ((eq prompt 'git-spice-branch-create-menu) '("--no-commit"))
                (t '())))))
    (should (equal (git-spice-arguments 'git-spice-branch-create-menu)
                   '("--no-commit")))))

;;; Integration Tests (with mocking, require git-spice)

(ert-deftest git-spice-test-section-error-handling ()
  "Test that function doesn't crash with malformed JSON."
  (with-temp-buffer
    (cl-letf (((symbol-function 'magit-toplevel)
                 (lambda () "/tmp/test-repo"))
                ((symbol-function 'shell-command-to-string)
                 (lambda (_) "{invalid json"))
                ((symbol-function 'magit-insert-heading)
                 (lambda (_) (insert "Stacks\n")))
                ((symbol-function 'magit-insert-section)
                 (lambda (_ &rest body) (apply #'funcall body)))
                ((symbol-function 'magit-insert-section-body)
                 (lambda (&rest body) (apply #'funcall body)))
                ((symbol-function 'propertize)
                 (lambda (str &rest _) str)))
      ;; Should not crash with invalid JSON
      (should-not (eq nil (git-spice-magit-insert-section)))
      ;; Buffer should contain something (header was inserted)
      (should (> (length (buffer-string)) 0)))))

;;; Command Tests (require git-spice)

(ert-deftest git-spice-test-run-command-success ()
  "Test git-spice-run with successful command."
  (let ((process-sentinel nil)
        (test-buffer (get-buffer-create "*gs-test*"))
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () temporary-file-directory))
              ((symbol-function 'start-process)
               (lambda (name buffer program &rest args)
                 (should (equal name "gs"))
                 (should (equal program "gs"))
                 (should (member "repo" args))
                 (should (member "sync" args))
                 ;; Return a fake process object instead of trying to execute
                 (list 'process 'mock)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ sentinel) (setq process-sentinel sentinel))))
      (git-spice-run "repo" "sync")
      (should process-sentinel))
    (when (buffer-live-p test-buffer)
      (kill-buffer test-buffer))))

(ert-deftest git-spice-test-run-command-error-handling ()
  "Test git-spice-run error handling when process fails to start."
  (let ((error-caught nil)
        (error-message-sent nil)
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () temporary-file-directory))
              ((symbol-function 'start-process)
               (lambda (&rest _)
                 (signal 'file-error '("Searching for program" "No such file or directory" "gs"))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when (string-match-p "Failed to start gs process" fmt)
                   (setq error-message-sent t)))))
      (condition-case err
          (git-spice-run "stack" "submit")
        (error
         (setq error-caught t)
         (should (string-match-p "Failed to start gs process" (format "%S" err))))))
    (should error-caught)
    (should error-message-sent)))

(ert-deftest git-spice-test-run-command-produces-debug-message ()
  "Test that git-spice-run produces debug message before running."
  (let ((messages-seen nil)
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () temporary-file-directory))
              ((symbol-function 'start-process)
               (lambda (_name _buffer _program &rest _args)
                 ;; Return a fake process object (just a list) instead of trying to execute
                 (list 'process 'mock)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _sentinel) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages-seen))))
      (git-spice-run "stack" "submit")
      ;; Check that we got a debug message
      (should messages-seen)
      (should (cl-some (lambda (msg) (string-match-p "Running: gs" msg))
                       messages-seen)))))

;;; Transient Menu Tests

(ert-deftest git-spice-test-stack-menu-definition ()
  "Test that stack menu actions have correct :transient settings.
This ensures actions exit the menu so users can see feedback."
  ;; Read the source file to verify :transient nil is present
  (with-temp-buffer
    (insert-file-contents (locate-library "git-spice.el"))
    (goto-char (point-min))
    ;; Find the git-spice-stack-menu definition
    (should (search-forward "(transient-define-prefix git-spice-stack-menu ()" nil t))
    ;; Find the Submit action and verify :transient nil
    (should (search-forward "\"Submit\"" nil t))
    (let ((search-end (+ (point) 200)))
      (should (search-forward ":transient nil" search-end t)))
    ;; Go back and find Restack action
    (goto-char (point-min))
    (search-forward "(transient-define-prefix git-spice-stack-menu ()" nil t)
    (search-forward "\"Restack\"" nil t)
    (let ((search-end (+ (point) 200)))
      (should (search-forward ":transient nil" search-end t)))))

(ert-deftest git-spice-test-branch-restack-menu-definition ()
  "Test that branch restack menu action has correct :transient setting.
This ensures the action exits the menu so users can see feedback."
  (with-temp-buffer
    (insert-file-contents (locate-library "git-spice.el"))
    (goto-char (point-min))
    ;; Find the git-spice-branch-restack-menu definition
    (should (search-forward "(transient-define-prefix git-spice-branch-restack-menu ()" nil t))
    ;; Find the Restack action
    (should (search-forward "\"Restack\"" nil t))
    ;; Search for :transient nil within the next 200 characters (same action definition)
    (let ((search-end (+ (point) 200)))
      (should (search-forward ":transient nil" search-end t)))))

;;; Branch Tree Insertion Tests

(ert-deftest git-spice-test-insert-branch-tree-basic ()
  "Test that a basic leaf branch renders name, tree chars, and symbol."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (branch `((name . "feature-1") (ups . nil) (commits . nil)))
           (branches-by-name (make-hash-table :test 'equal)))
      (puthash "feature-1" branch branches-by-name)
      (git-spice-insert-branch-tree branch branches-by-name "" t)
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "┗━" content))
        (should (string-match-p "□" content))
        (should (string-match-p "feature-1" content))))))

(ert-deftest git-spice-test-insert-branch-tree-current ()
  "Test that the current branch uses the filled square symbol."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (branch `((name . "my-branch") (current . t) (ups . nil) (commits . nil)))
           (branches-by-name (make-hash-table :test 'equal)))
      (puthash "my-branch" branch branches-by-name)
      (git-spice-insert-branch-tree branch branches-by-name "" t)
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "■" content))
        (should (string-match-p "my-branch" content))))))

(ert-deftest git-spice-test-insert-branch-tree-with-pr ()
  "Test that a branch with a change renders the PR id."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (branch `((name . "pr-branch")
                     (change . ((id . "#123") (url . "https://example.com/pr/123")))
                     (ups . nil) (commits . nil)))
           (branches-by-name (make-hash-table :test 'equal)))
      (puthash "pr-branch" branch branches-by-name)
      (git-spice-insert-branch-tree branch branches-by-name "" t)
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "#123" content))
        (should (string-match-p "pr-branch" content))))))

(ert-deftest git-spice-test-insert-branch-tree-needs-push ()
  "Test that a branch with push.ahead > 0 shows needs push."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (branch `((name . "push-branch")
                     (push . ((ahead . 2)))
                     (ups . nil) (commits . nil)))
           (branches-by-name (make-hash-table :test 'equal)))
      (puthash "push-branch" branch branches-by-name)
      (git-spice-insert-branch-tree branch branches-by-name "" t)
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "(needs push)" content))))))

(ert-deftest git-spice-test-insert-branch-tree-with-commits ()
  "Test that commits render with short hash and subject."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (branch `((name . "commit-branch")
                     (ups . nil)
                     (commits . (((sha . "abc123def456") (subject . "Fix the thing"))))))
           (branches-by-name (make-hash-table :test 'equal)))
      (puthash "commit-branch" branch branches-by-name)
      (git-spice-insert-branch-tree branch branches-by-name "" t)
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "abc123d" content))
        (should (string-match-p "Fix the thing" content))))))

;;; Process Sentinel Tests

(ert-deftest git-spice-test-sentinel-success ()
  "Test that exit 0 + finished triggers success message and magit-refresh."
  (let ((captured-sentinel nil)
        (refresh-called nil)
        (success-message nil)
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () temporary-file-directory))
              ((symbol-function 'start-process)
               (lambda (_name _buffer _program &rest _args)
                 (list 'process 'mock)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ sentinel) (setq captured-sentinel sentinel)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (let ((msg (apply #'format fmt args)))
                   (when (string-match-p "succeeded" msg)
                     (setq success-message msg))))))
      (git-spice-run "repo" "sync")
      (should captured-sentinel)
      ;; Now invoke the sentinel with a fake process
      (cl-letf (((symbol-function 'process-exit-status)
                 (lambda (_) 0))
                ((symbol-function 'magit-refresh)
                 (lambda () (setq refresh-called t))))
        (funcall captured-sentinel 'fake-proc "finished\n"))
      (should success-message)
      (should refresh-called))))

(ert-deftest git-spice-test-sentinel-failure ()
  "Test that exit 1 + finished triggers failure message and buffer display."
  (let ((captured-sentinel nil)
        (failure-message nil)
        (buffer-displayed nil)
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () temporary-file-directory))
              ((symbol-function 'start-process)
               (lambda (_name _buffer _program &rest _args)
                 (list 'process 'mock)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ sentinel) (setq captured-sentinel sentinel)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (let ((msg (apply #'format fmt args)))
                   (when (string-match-p "failed" msg)
                     (setq failure-message msg))))))
      (git-spice-run "stack" "submit")
      (should captured-sentinel)
      (let ((gs-buf (get-buffer-create "*gs*")))
        (cl-letf (((symbol-function 'process-exit-status)
                   (lambda (_) 1))
                  ((symbol-function 'display-buffer)
                   (lambda (_) (setq buffer-displayed t))))
          (funcall captured-sentinel 'fake-proc "finished\n"))
        (should failure-message)
        (should buffer-displayed)
        (kill-buffer gs-buf)))))

(ert-deftest git-spice-test-sentinel-other-event ()
  "Test that a non-finished event produces a generic message."
  (let ((captured-sentinel nil)
        (event-message nil)
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () temporary-file-directory))
              ((symbol-function 'start-process)
               (lambda (_name _buffer _program &rest _args)
                 (list 'process 'mock)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ sentinel) (setq captured-sentinel sentinel)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (let ((msg (apply #'format fmt args)))
                   (when (string-match-p "hangup" msg)
                     (setq event-message msg))))))
      (git-spice-run "branch" "track")
      (should captured-sentinel)
      (cl-letf (((symbol-function 'process-exit-status)
                 (lambda (_) 0)))
        (funcall captured-sentinel 'fake-proc "hangup\n"))
      (should event-message))))

;;; Hook Setup/Teardown Tests

(ert-deftest git-spice-test-setup-magit-section ()
  "Test that setup calls add-hook with the correct arguments."
  (let ((hook-args nil))
    (cl-letf (((symbol-function 'add-hook)
               (lambda (&rest args) (setq hook-args args))))
      (git-spice-setup-magit-section)
      (should hook-args)
      (should (eq (nth 0 hook-args) 'magit-status-sections-hook))
      (should (eq (nth 1 hook-args) 'git-spice-magit-insert-section))
      (should (eq (nth 2 hook-args) t)))))

(ert-deftest git-spice-test-teardown-magit-section ()
  "Test that teardown calls remove-hook with the correct arguments."
  (let ((hook-args nil))
    (cl-letf (((symbol-function 'remove-hook)
               (lambda (&rest args) (setq hook-args args))))
      (git-spice-teardown-magit-section)
      (should hook-args)
      (should (eq (nth 0 hook-args) 'magit-status-sections-hook))
      (should (eq (nth 1 hook-args) 'git-spice-magit-insert-section))
      (should (eq (nth 2 hook-args) t)))))

;;; Command Wrapper Tests

(ert-deftest git-spice-test-repo-sync-command ()
  "Test that repo-sync passes correct args to git-spice-run."
  (let ((captured-args nil)
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'git-spice-run)
               (lambda (&rest args) (setq captured-args args)))
              ((symbol-function 'transient-args)
               (lambda (_) '("--restack"))))
      (git-spice-repo-sync)
      (should (equal captured-args '("repo" "sync" "--restack"))))))

(ert-deftest git-spice-test-branch-create-command ()
  "Test that branch-create passes correct args and name."
  (let ((captured-args nil)
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'git-spice-run)
               (lambda (&rest args) (setq captured-args args)))
              ((symbol-function 'transient-args)
               (lambda (_) '("--no-commit"))))
      (git-spice-branch-create "my-feature")
      (should (equal captured-args '("branch" "create" "--no-commit" "my-feature"))))))

(ert-deftest git-spice-test-branch-checkout-command ()
  "Test that branch-checkout passes correct branch name."
  (let ((captured-args nil)
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'git-spice-run)
               (lambda (&rest args) (setq captured-args args)))
              ((symbol-function 'magit-read-branch)
               (lambda (_) "other-branch")))
      (git-spice-branch-checkout)
      (should (equal captured-args '("branch" "checkout" "other-branch"))))))

(ert-deftest git-spice-test-log-commands ()
  "Test that log-short and log-long call run-display with correct args."
  (let ((captured-args nil)
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'git-spice-run-display)
               (lambda (&rest args) (setq captured-args args))))
      (git-spice-log-short)
      (should (equal captured-args '("log" "short")))
      (git-spice-log-long)
      (should (equal captured-args '("log" "long"))))))

(provide 'git-spice-test)
;;; git-spice-test.el ends here
