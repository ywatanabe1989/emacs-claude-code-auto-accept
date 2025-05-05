;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:35:56>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-emacs-claude-code-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'emacs-claude-code-send)
(require 'emacs-claude-code-detect-prompt)

(ert-deftest test-emacs-claude-code-send-loadable ()
  (should (featurep 'emacs-claude-code-send)))

(ert-deftest test-emacs-claude-code-send-defined ()
  (should (fboundp 'emacs-claude-code-send)))

(ert-deftest test-emacs-claude-auto-send-y-defined ()
  (should (fboundp '--emacs-claude-auto-send-y)))

(ert-deftest test-emacs-claude-auto-send-yy-defined ()
  (should (fboundp '--emacs-claude-auto-send-yy)))

(ert-deftest test-emacs-claude-auto-send-continue-defined ()
  (should (fboundp '--emacs-claude-auto-send-continue)))

(ert-deftest test-emacs-claude-auto-send-y-sends-correct-response ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (prompt-detected nil)
        (string-sent nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)
          (with-current-buffer mock-buffer
            (insert "Some content\n❯ 1. Yes\nMore content"))

          (cl-letf
              (((symbol-function '--emacs-claude-detect-prompt-y/n)
                (lambda () (setq prompt-detected t) t))
               ((symbol-function 'vterm-send-string)
                (lambda (string) (setq string-sent string)))
               ((symbol-function 'vterm-send-return) #'ignore)
               ((symbol-function 'vterm-copy-mode) #'ignore)
               ((symbol-function 'sit-for) #'ignore))

            (--emacs-claude-auto-send-y)
            (should prompt-detected)
            (should (string= string-sent "1\n"))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest test-emacs-claude-auto-send-yy-sends-correct-response ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (prompt-detected nil)
        (string-sent nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)
          (with-current-buffer mock-buffer
            (insert
             "Some content\n 2. Yes, and don't ask again\nMore content"))

          (cl-letf
              (((symbol-function '--emacs-claude-detect-prompt-y/y/n)
                (lambda () (setq prompt-detected t) t))
               ((symbol-function 'vterm-send-string)
                (lambda (string) (setq string-sent string)))
               ((symbol-function 'vterm-send-return) #'ignore)
               ((symbol-function 'vterm-copy-mode) #'ignore)
               ((symbol-function 'sit-for) #'ignore))

            (--emacs-claude-auto-send-yy)
            (should prompt-detected)
            (should (string= string-sent "2\n"))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest
    test-emacs-claude-auto-send-continue-sends-correct-response ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (prompt-detected nil)
        (string-sent nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)
          (with-current-buffer mock-buffer
            (insert "Some content\n│ > |More content"))

          (cl-letf
              (((symbol-function '--emacs-claude-detect-prompt-waiting)
                (lambda () (setq prompt-detected t) t))
               ((symbol-function
                 '--emacs-claude-detect-prompt-initial-waiting)
                (lambda () nil))
               ((symbol-function 'vterm-send-string)
                (lambda (string) (setq string-sent string)))
               ((symbol-function 'vterm-send-return) #'ignore)
               ((symbol-function 'vterm-copy-mode) #'ignore)
               ((symbol-function 'sit-for) #'ignore))

            (--emacs-claude-auto-send-continue)
            (should prompt-detected)
            (should (string= string-sent "continue\n"))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest test-emacs-claude-code-send-routes-to-correct-handler ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (handled-by nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)

          (cl-letf
              (((symbol-function '--emacs-claude-detect-prompt-y/y/n)
                (lambda () nil))
               ((symbol-function
                 '--emacs-claude-detect-prompt-y/n)
                (lambda () t))
               ((symbol-function
                 '--emacs-claude-detect-prompt-waiting)
                (lambda () nil))
               ((symbol-function
                 '--emacs-claude-detect-prompt-initial-waiting)
                (lambda () nil))
               ((symbol-function '--emacs-claude-auto-send-y)
                (lambda () (setq handled-by 'y)))
               ((symbol-function '--emacs-claude-auto-send-yy)
                (lambda () (setq handled-by 'yy)))
               ((symbol-function
                 '--emacs-claude-auto-send-continue)
                (lambda () (setq handled-by 'continue)))
               ((symbol-function 'vterm-clear) #'ignore))

            (emacs-claude-code-send)
            (should (eq handled-by 'y))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))


(provide 'test-emacs-claude-code-send)

(when
    (not load-file-name)
  (message "test-emacs-claude-code-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))