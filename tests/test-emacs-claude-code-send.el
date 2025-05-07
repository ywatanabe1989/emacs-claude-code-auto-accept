;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:35:56>
;;; File: /home/ywatanabe/.emacs.d/lisp/ecc/tests/test-ecc-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-send)
(require 'ecc-detect-prompt)

(ert-deftest test-ecc-send-loadable ()
  (should (featurep 'ecc-send-accept)))

(ert-deftest test-ecc-send-defined ()
  (should (fboundp 'ecc-send-accept)))

(ert-deftest test-ecc-auto-send-y-defined ()
  (should (fboundp '--ecc-auto-send-1-y/n)))

(ert-deftest test-ecc-auto-send-yy-defined ()
  (should (fboundp '--ecc-auto-send-2-y/y/n)))

(ert-deftest test-ecc-auto-send-continue-defined ()
  (should (fboundp '--ecc-auto-send-continue)))

(ert-deftest test-ecc-auto-send-y-sends-correct-response ()
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (prompt-detected nil)
        (string-sent nil))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)
          (with-current-buffer mock-buffer
            (insert "Some content\n❯ 1. Yes\nMore content"))

          (cl-letf
              (((symbol-function '--ecc-state-y/n-p)
                (lambda () (setq prompt-detected t) t))
               ((symbol-function 'vterm-send-string)
                (lambda (string) (setq string-sent string)))
               ((symbol-function 'vterm-send-return) #'ignore)
               ((symbol-function 'vterm-copy-mode) #'ignore)
               ((symbol-function 'sit-for) #'ignore))

            (--ecc-auto-send-1-y/n)
            (should prompt-detected)
            (should (string= string-sent "1\n"))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer))))

(ert-deftest test-ecc-auto-send-yy-sends-correct-response ()
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (prompt-detected nil)
        (string-sent nil))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)
          (with-current-buffer mock-buffer
            (insert
             "Some content\n 2. Yes, and don't ask again\nMore content"))

          (cl-letf
              (((symbol-function '--ecc-state-y/y/n-p)
                (lambda () (setq prompt-detected t) t))
               ((symbol-function 'vterm-send-string)
                (lambda (string) (setq string-sent string)))
               ((symbol-function 'vterm-send-return) #'ignore)
               ((symbol-function 'vterm-copy-mode) #'ignore)
               ((symbol-function 'sit-for) #'ignore))

            (--ecc-auto-send-2-y/y/n)
            (should prompt-detected)
            (should (string= string-sent "2\n"))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer))))

(ert-deftest
    test-ecc-auto-send-continue-sends-correct-response ()
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (prompt-detected nil)
        (string-sent nil))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)
          (with-current-buffer mock-buffer
            (insert "Some content\n│ > |More content"))

          (cl-letf
              (((symbol-function '--ecc-state-waiting-p)
                (lambda () (setq prompt-detected t) t))
               ((symbol-function
                 '--ecc-state-initial-waiting-p)
                (lambda () nil))
               ((symbol-function 'vterm-send-string)
                (lambda (string) (setq string-sent string)))
               ((symbol-function 'vterm-send-return) #'ignore)
               ((symbol-function 'vterm-copy-mode) #'ignore)
               ((symbol-function 'sit-for) #'ignore))

            (--ecc-auto-send-continue)
            (should prompt-detected)
            (should (string= string-sent "continue\n"))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer))))

(ert-deftest test-ecc-send-routes-to-correct-handler ()
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (handled-by nil))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)

          (cl-letf
              (((symbol-function '--ecc-state-y/y/n-p)
                (lambda () nil))
               ((symbol-function
                 '--ecc-state-y/n-p)
                (lambda () t))
               ((symbol-function
                 '--ecc-state-waiting-p)
                (lambda () nil))
               ((symbol-function
                 '--ecc-state-initial-waiting-p)
                (lambda () nil))
               ((symbol-function '--ecc-auto-send-1-y/n)
                (lambda () (setq handled-by 'y)))
               ((symbol-function '--ecc-auto-send-2-y/y/n)
                (lambda () (setq handled-by 'yy)))
               ((symbol-function
                 '--ecc-auto-send-continue)
                (lambda () (setq handled-by 'continue)))
               ((symbol-function 'vterm-clear) #'ignore))

            (ecc-send-accept)
            (should (eq handled-by 'y))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer))))


(provide 'test-ecc-send)

(when
    (not load-file-name)
  (message "test-ecc-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))