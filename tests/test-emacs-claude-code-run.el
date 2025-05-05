;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:35:18>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-emacs-claude-code-run.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'emacs-claude-code-run)

(ert-deftest test-emacs-claude-code-run-loadable ()
  (should (featurep 'emacs-claude-code-run)))

(ert-deftest test-emacs-claude-prompt-template-defined ()
  (should (boundp 'emacs-claude-prompt-template))
  (should (stringp emacs-claude-prompt-template)))

(ert-deftest test-emacs-claude-default-instructions-defined ()
  (should (boundp 'emacs-claude-default-instructions))
  (should (stringp emacs-claude-default-instructions)))

(ert-deftest test-emacs-claude-ensure-buffer-defined ()
  (should (fboundp 'emacs-claude-ensure-buffer)))

(ert-deftest test-emacs-claude-run-defined ()
  (should (fboundp 'emacs-claude-run)))

(ert-deftest test-emacs-claude-run-on-region-defined ()
  (should (fboundp 'emacs-claude-run-on-region)))

(ert-deftest test-emacs-claude-run-on-buffer-defined ()
  (should (fboundp 'emacs-claude-run-on-buffer)))

(ert-deftest test-emacs-claude-run-quick-defined ()
  (should (fboundp 'emacs-claude-run-quick)))

(ert-deftest test-emacs-claude-run-formats-prompt-correctly ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (test-prompt "Test prompt")
        (test-content "Test content")
        (formatted-prompt nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)
          (cl-letf (((symbol-function 'vterm-send-string)
                     (lambda (string) (setq formatted-prompt string)))
                    ((symbol-function 'vterm-send-return) #'ignore)
                    ((symbol-function 'vterm-clear) #'ignore)
                    ((symbol-function 'sit-for) #'ignore))
            (emacs-claude-run test-prompt test-content)
            (should (string= formatted-prompt
                             (format emacs-claude-prompt-template
                                     (concat test-prompt "\n\n"
                                             test-content))))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest test-emacs-claude-run-on-region-extracts-region-content
    ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (test-buffer (generate-new-buffer "*TEST-REGION*"))
        (prompt-used nil)
        (content-used nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)
          (with-current-buffer test-buffer
            (insert "Test content for region")
            (cl-letf (((symbol-function 'emacs-claude-run)
                       (lambda (prompt content)
                         (setq prompt-used prompt
                               content-used content))))
              (emacs-claude-run-on-region 1 10 "Test prompt")
              (should (string= prompt-used "Test prompt"))
              (should (string= content-used "Test cont")))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest test-emacs-claude-run-on-buffer-extracts-buffer-content
    ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (test-buffer (generate-new-buffer "*TEST-BUFFER*"))
        (prompt-used nil)
        (content-used nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)
          (with-current-buffer test-buffer
            (insert "Test buffer content")
            (cl-letf (((symbol-function 'emacs-claude-run)
                       (lambda (prompt content)
                         (setq prompt-used prompt
                               content-used content))))
              (emacs-claude-run-on-buffer "Test prompt")
              (should (string-match-p "Test prompt" prompt-used))
              (should
               (string-match-p
                (regexp-quote (buffer-name test-buffer)) prompt-used))
              (should (string= content-used "Test buffer content")))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq emacs-claude-buffer orig-buffer))))


(provide 'test-emacs-claude-code-run)

(when
    (not load-file-name)
  (message "test-emacs-claude-code-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))