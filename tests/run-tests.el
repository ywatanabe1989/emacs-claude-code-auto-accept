;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:06>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/run-tests.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)

(defun ecc-run-tests (&optional pattern)
  "Run all ecc tests matching PATTERN.
If PATTERN is nil, run all tests with 'test-ecc' prefix."
  (interactive)
  (let ((test-pattern (or pattern "^test-ecc")))
    (ert-run-tests-interactively test-pattern)))

(defun ecc-load-tests ()
  "Load all ecc test files."
  (interactive)
  (let
      ((test-dir
        "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests"))
    (unless (file-exists-p test-dir)
      (error "Test directory not found: %s" test-dir))
    (dolist (test-file (directory-files test-dir t "^test-.*\\.el$"))
      (load test-file))))

(defun ecc-run-single-test-file (file-pattern)
  "Run tests from a single test file matching FILE-PATTERN."
  (interactive "sEnter test file pattern (e.g. variables): ")
  (let*
      ((test-dir
        "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests")
       (pattern (concat "^test-ecc-" file-pattern))
       (matching-files (directory-files test-dir t pattern)))
    (if matching-files
        (progn
          (dolist (file matching-files)
            (load file))
          (ert-run-tests-interactively pattern))
      (message "No test files matching %s found" pattern))))

(when (and (not load-file-name)
           (not noninteractive))
  (ecc-load-tests)
  (ecc-run-tests))


(provide 'run-tests)

(when
    (not load-file-name)
  (message "run-tests.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))