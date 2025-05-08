;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 22:35:00>
;;; File: elisp-project-tests.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; A versatile Emacs Lisp test runner for standardized project structures.
;; This can be symlinked into any Emacs Lisp project with a similar structure.
;;
;; Usage from Emacs:
;;   M-x load-file RET path/to/elisp-project-tests.el RET
;;   M-x elisp-project-run-tests RET
;;
;; Usage from command line:
;;   emacs -batch -l path/to/elisp-project-tests.el --eval "(elisp-project-run-tests-batch)"
;;
;; Features:
;; - Auto-detects project root and test files
;; - Works with any project prefix (not hardcoded to "ecc-")
;; - Supports selective testing with patterns
;; - Generates reports in various formats

(require 'ert)

(defgroup elisp-project-tests nil
  "Customization group for Elisp project test runner."
  :group 'tools
  :prefix "elisp-project-tests-")

(defvar elisp-project-tests-pattern nil
  "Pattern for test files to load and run.")

(defvar elisp-project-tests-prefix nil
  "Project-specific prefix for test files (e.g., 'ecc-').")

(defvar elisp-project-tests-test-dir nil
  "Directory containing test files.")

(defvar elisp-project-tests-project-root nil
  "Root directory of the project.")

(defvar elisp-project-tests-module-dirs nil
  "List of module directories within the project.")

(defvar elisp-project-tests-results nil
  "Results of the last test run.")

;;;###autoload
(defun elisp-project-detect-environment ()
  "Detect the project environment automatically.
Sets project root, test directory, and project prefix."
  (interactive)
  ;; Find the project root (current directory or parent with .git)
  (setq elisp-project-tests-project-root
        (locate-dominating-file default-directory ".git"))
  
  ;; If not found, use current directory
  (unless elisp-project-tests-project-root
    (setq elisp-project-tests-project-root default-directory))
  
  ;; Find the tests directory
  (setq elisp-project-tests-test-dir
        (expand-file-name "tests" elisp-project-tests-project-root))
  
  ;; If tests directory doesn't exist, try alternatives
  (unless (file-directory-p elisp-project-tests-test-dir)
    (dolist (alt-name '("test" "t"))
      (let ((alt-dir (expand-file-name alt-name elisp-project-tests-project-root)))
        (when (file-directory-p alt-dir)
          (setq elisp-project-tests-test-dir alt-dir)))))
  
  ;; Find module directories (any subdirectory in project root with .el files)
  (setq elisp-project-tests-module-dirs
        (cl-remove-if-not
         (lambda (dir)
           (and (file-directory-p dir)
                (directory-files dir t "\\.el$" t)))
         (directory-files elisp-project-tests-project-root t "^[^.]")))
  
  ;; Detect project prefix from test files
  (let* ((test-files (directory-files elisp-project-tests-test-dir t "^test-.*\\.el$"))
         (prefixes (mapcar (lambda (file)
                             (let ((name (file-name-nondirectory file)))
                               (when (string-match "^test-\\([^-]+\\)-" name)
                                 (match-string 1 name))))
                           test-files))
         (non-nil-prefixes (cl-remove-if #'null prefixes))
         (prefix-counts (make-hash-table :test 'equal)))
    
    ;; Count occurrences of each prefix
    (dolist (prefix non-nil-prefixes)
      (puthash prefix (1+ (or (gethash prefix prefix-counts) 0)) prefix-counts))
    
    ;; Find the most common prefix
    (let ((max-count 0)
          (most-common-prefix nil))
      (maphash (lambda (prefix count)
                 (when (> count max-count)
                   (setq max-count count)
                   (setq most-common-prefix prefix)))
               prefix-counts)
      
      (setq elisp-project-tests-prefix most-common-prefix)))
  
  ;; If we couldn't detect a prefix, use a sane default
  (unless elisp-project-tests-prefix
    (setq elisp-project-tests-prefix
          (file-name-nondirectory
           (directory-file-name elisp-project-tests-project-root))))
  
  ;; Report what we found
  (message "Project root: %s" elisp-project-tests-project-root)
  (message "Tests directory: %s" elisp-project-tests-test-dir)
  (message "Project prefix: %s" elisp-project-tests-prefix)
  (message "Module directories: %s" elisp-project-tests-module-dirs))

;;;###autoload
(defun elisp-project-load-tests (&optional test-pattern)
  "Load all test files matching TEST-PATTERN.
If TEST-PATTERN is nil, load all test files in the test directory."
  (interactive)
  ;; Detect environment if needed
  (unless elisp-project-tests-test-dir
    (elisp-project-detect-environment))
  
  ;; Expand load path to include all project directories
  (add-to-list 'load-path elisp-project-tests-project-root)
  (dolist (dir elisp-project-tests-module-dirs)
    (add-to-list 'load-path dir))
  (add-to-list 'load-path elisp-project-tests-test-dir)
  
  ;; Determine which test files to load
  (let* ((pattern (or test-pattern "^test-"))
         (test-files (directory-files elisp-project-tests-test-dir t pattern)))
    
    ;; Load mock files first (if any)
    (dolist (file (directory-files elisp-project-tests-test-dir t "mock.*\\.el$"))
      (load file))
    
    ;; Then load test files
    (dolist (file test-files)
      (when (and (string-match "\\.el$" file)
                 (not (string-match "~$" file))) ; Skip backup files
        (condition-case err
            (load file)
          (error
           (message "Error loading %s: %s" file (error-message-string err))))))))

;;;###autoload
(defun elisp-project-run-tests (&optional pattern)
  "Run all tests matching PATTERN.
If PATTERN is nil, run all tests for the detected project."
  (interactive)
  (unless elisp-project-tests-prefix
    (elisp-project-detect-environment))
  
  (elisp-project-load-tests)
  
  (let ((test-pattern (or pattern (concat "^test-" elisp-project-tests-prefix))))
    (message "Running tests matching: %s" test-pattern)
    (ert-run-tests-interactively test-pattern)))

;;;###autoload
(defun elisp-project-run-tests-batch ()
  "Run all tests in batch mode. Used for command-line testing."
  (unless elisp-project-tests-prefix
    (elisp-project-detect-environment))
  
  ;; Parse command line arguments
  (let ((args command-line-args-left)
        (pattern nil))
    (while args
      (cond
       ((string= (car args) "--pattern")
        (setq args (cdr args))
        (setq pattern (car args)))
       ;; Add more options as needed
       )
      (setq args (cdr args)))
    
    ;; Load and run tests
    (elisp-project-load-tests)
    (setq elisp-project-tests-pattern
          (or pattern (concat "^test-" elisp-project-tests-prefix)))
    
    (message "Running tests matching: %s" elisp-project-tests-pattern)
    (ert-run-tests-batch elisp-project-tests-pattern)))

;;;###autoload
(defun elisp-project-run-single-test-file (file-pattern)
  "Run tests from a single test file matching FILE-PATTERN."
  (interactive "sEnter test file pattern: ")
  (unless elisp-project-tests-prefix
    (elisp-project-detect-environment))
  
  (let* ((pattern (concat "^test-" elisp-project-tests-prefix "-" file-pattern))
         (matching-files (directory-files elisp-project-tests-test-dir t pattern)))
    (if matching-files
        (progn
          (dolist (file matching-files)
            (load file))
          (ert-run-tests-interactively 
           (concat "^test-" elisp-project-tests-prefix "-" file-pattern)))
      (message "No test files matching %s found" pattern))))

;;;###autoload
(defun elisp-project-generate-report ()
  "Generate a test report in org-mode format."
  (interactive)
  (unless elisp-project-tests-results
    (setq elisp-project-tests-results
          (ert-run-tests-batch elisp-project-tests-pattern)))
  
  (with-current-buffer (get-buffer-create "*Test Report*")
    (erase-buffer)
    (org-mode)
    (insert "#+TITLE: Elisp Test Report\n")
    (insert "#+AUTHOR: Generated by elisp-project-tests\n")
    (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
    
    (insert "* Test Summary\n\n")
    (let* ((stats (ert-stats-completed elisp-project-tests-results))
           (total (ert-stats-total stats))
           (passed (ert-stats-passed stats))
           (failed (ert-stats-failed stats))
           (skipped (ert-stats-error stats))
           (percentage (if (> total 0)
                          (* 100.0 (/ (float passed) total))
                        0.0)))
      (insert (format "- Passed: %d\n" passed))
      (insert (format "- Failed: %d\n" failed))
      (insert (format "- Skipped/Errors: %d\n" skipped))
      (insert (format "- Total: %d\n" total))
      (insert (format "- Success Rate: %.1f%%\n\n" percentage)))
    
    (when (> (ert-stats-completed elisp-project-tests-results) 0)
      (insert "* Passed Tests\n\n")
      ;; Insert passed tests here
      
      (when (> (ert-stats-failed elisp-project-tests-results) 0)
        (insert "* Failed Tests\n\n")
        ;; Insert failed tests here
        ))
    
    (display-buffer (current-buffer))))

;; Auto-run when loaded as a script
(when (and (not load-file-name)
           (not noninteractive))
  (elisp-project-run-tests))

(provide 'elisp-project-tests)

(when (not load-file-name)
  (message "elisp-project-tests.el loaded."))