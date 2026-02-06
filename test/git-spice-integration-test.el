;;; git-spice-integration-test.el --- Integration tests for git-spice.el -*- lexical-binding: t -*-

;;; Commentary:

;; Integration tests for git-spice.el that run actual gs commands
;; in temporary git repositories. These tests are skipped if gs is not installed.

;;; Code:

(require 'ert)
(let ((test-dir (expand-file-name (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path test-dir))
(require 'git-spice)
(require 'test-helpers)

(ert-deftest gs-integration-branch-create ()
  "Test creating a branch with gs branch create using git-spice-run."
  (gs-integration-skip-unless)
  (with-temp-git-repo
    (with-gs-initialised
      ;; Mock magit-toplevel and magit-refresh to avoid dependencies
      (cl-letf (((symbol-function 'magit-toplevel) (lambda () default-directory))
                ((symbol-function 'magit-refresh) (lambda () nil))
                ((symbol-function 'transient-args) (lambda (_) '())))
        ;; Call git-spice-run which spawns gs async
        (git-spice-run "branch" "create" "-m" "add feature")
        ;; Wait for async completion
        (should (gs-integration-wait-for-gs-process 10))
        ;; Verify the branch was created
        (should (gs-integration-branch-exists-p "add-feature"))))))

(ert-deftest gs-integration-branch-delete ()
  "Test deleting a branch using git-spice-run."
  (gs-integration-skip-unless)
  (with-temp-git-repo
    (with-gs-initialised
      ;; Mock magit-toplevel and magit-refresh to avoid dependencies
      (cl-letf (((symbol-function 'magit-toplevel) (lambda () default-directory))
                ((symbol-function 'magit-refresh) (lambda () nil))
                ((symbol-function 'transient-args) (lambda (_) '())))
        ;; Create the branch first
        (git-spice-run "branch" "create" "-m" "delete me")
        (should (gs-integration-wait-for-gs-process 10))
        (should (gs-integration-branch-exists-p "delete-me"))
        ;; Delete the branch
        (git-spice-run "branch" "delete" "--force" "delete-me")
        (should (gs-integration-wait-for-gs-process 10))
        (should-not (gs-integration-branch-exists-p "delete-me"))))))

(provide 'git-spice-integration-test.el)
;;; git-spice-integration-test.el ends here
