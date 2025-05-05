;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:37:20>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-emacs-claude-code-copy-repository.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'cl-lib)

;; Mock magit for testing
(unless (fboundp 'magit-toplevel)
  (defun magit-toplevel (&optional directory)
    "Mock implementation of magit-toplevel for testing."
    (or directory default-directory)))

(require 'emacs-claude-code-copy-repository)

(ert-deftest test-emacs-claude-code-copy-repository-loadable ()
  (should (featurep 'emacs-claude-code-copy-repository)))

(ert-deftest test-emacs-claude-repository-dir-defined ()
  (should (boundp 'emacs-claude-repository-dir)))

(ert-deftest test-emacs-claude-repository-output-file-defined ()
  (should (boundp 'emacs-claude-repository-output-file))
  (should (stringp emacs-claude-repository-output-file)))

(ert-deftest test-emacs-claude-file-blacklist-defined ()
  (should (boundp 'emacs-claude-file-blacklist))
  (should (listp emacs-claude-file-blacklist)))

(ert-deftest test-emacs-claude-max-file-size-defined ()
  (should (boundp 'emacs-claude-max-file-size))
  (should (integerp emacs-claude-max-file-size)))

(ert-deftest test-emacs-claude-blacklisted-p-functionality ()
  (should (emacs-claude-blacklisted-p "/path/to/.git/config"))
  (should (emacs-claude-blacklisted-p "/path/to/image.png"))
  (should-not (emacs-claude-blacklisted-p "/path/to/code.el")))

(ert-deftest test-emacs-claude-get-file-type-functionality ()
  (should
   (string= (emacs-claude-get-file-type "/path/to/file.el") "elisp"))
  (should
   (string= (emacs-claude-get-file-type "/path/to/file.py") "python"))
  (should
   (string= (emacs-claude-get-file-type "/path/to/file.js")
            "javascript"))
  (should (string= (emacs-claude-get-file-type "/path/to/file.c") "c"))
  (should
   (string= (emacs-claude-get-file-type "/path/to/file.unknown")
            "unknown")))

(ert-deftest test-emacs-claude-get-file-content ()
  (let ((temp-file (make-temp-file "emacs-claude-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "Test file content"))
          (should
           (string= (emacs-claude-get-file-content temp-file)
                    "Test file content")))
      (delete-file temp-file))))

(ert-deftest test-emacs-claude-get-repository-files-with-mock-dir ()
  (let* ((temp-dir (make-temp-file "emacs-claude-test-dir-" t))
         (subdir
          (file-name-as-directory (expand-file-name "subdir" temp-dir)))
         (elisp-file (expand-file-name "test.el" temp-dir))
         (python-file (expand-file-name "test.py" subdir))
         (image-file (expand-file-name "test.png" temp-dir))
         (large-file (expand-file-name "large.txt" temp-dir))
         (orig-max-size emacs-claude-max-file-size))
    (unwind-protect
        (progn
          (make-directory subdir)
          (with-temp-file elisp-file
            (insert "(defun test () (message \"test\"))"))
          (with-temp-file python-file
            (insert "def test():\n    print('test')"))
          (with-temp-file image-file
            (insert "fake image content"))
          (with-temp-file large-file
            (insert (make-string (1+ emacs-claude-max-file-size) ?x)))

          (let ((files (emacs-claude-get-repository-files temp-dir)))
            ;; Should include elisp and python files
            (should (member elisp-file files))
            (should (member python-file files))
            ;; Should exclude image file and large file
            (should-not (member image-file files))
            (should-not (member large-file files))))
      (delete-directory temp-dir t))))

(ert-deftest test-emacs-claude-copy-repository-creates-output-file ()
  (let* ((temp-dir (make-temp-file "emacs-claude-test-repo-" t))
         (test-file (expand-file-name "test.el" temp-dir))
         (expected-output-path
          (expand-file-name emacs-claude-repository-output-file
                            temp-dir))
         (orig-repository-dir emacs-claude-repository-dir)
         (kill-called nil)
         (kill-content nil))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () (message \"test\"))"))

          (cl-letf (((symbol-function 'kill-new)
                     (lambda (content)
                       (setq kill-called t
                             kill-content content))))
            (emacs-claude-copy-repository temp-dir)

            ;; Check if output file was created
            (should (file-exists-p expected-output-path))

            ;; Check if content was added to kill ring
            (should kill-called)
            (should (stringp kill-content))
            (should
             (string-match-p "Repository Structure" kill-content))
            (should
             (string-match-p
              (regexp-quote "(defun test () (message \"test\"))")
              kill-content))

            ;; Check if repository dir was remembered
            (should (string= emacs-claude-repository-dir temp-dir))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t))
      (setq emacs-claude-repository-dir orig-repository-dir))))


(provide 'test-emacs-claude-code-copy-repository)

(when
    (not load-file-name)
  (message "test-emacs-claude-code-copy-repository.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))