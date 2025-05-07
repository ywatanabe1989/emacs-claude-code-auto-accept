;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:35:18>
;;; File: /home/ywatanabe/.emacs.d/lisp/ecc/tests/test-ecc-run.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-run)

(ert-deftest test-ecc-run-loadable ()
  (should (featurep 'ecc-run)))

(ert-deftest test-ecc-prompt-template-defined ()
  (should (boundp 'ecc-prompt-template))
  (should (stringp ecc-prompt-template)))

(ert-deftest test-ecc-default-instructions-defined ()
  (should (boundp 'ecc-default-instructions))
  (should (stringp ecc-default-instructions)))

(ert-deftest test-ecc-buffer-ensure-active-exists-defined ()
  (should (fboundp 'ecc-buffer-ensure-active-exists)))

(ert-deftest test-ecc-run-defined ()
  (should (fboundp 'ecc-run)))

(ert-deftest test-ecc-run-on-region-defined ()
  (should (fboundp 'ecc-run-on-region)))

(ert-deftest test-ecc-run-on-buffer-defined ()
  (should (fboundp 'ecc-run-on-buffer)))

(ert-deftest test-ecc-run-quick-defined ()
  (should (fboundp 'ecc-run-quick)))

(ert-deftest test-ecc-run-formats-prompt-correctly ()
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (test-prompt "Test prompt")
        (test-content "Test content")
        (formatted-prompt nil))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)
          (cl-letf (((symbol-function 'vterm-send-string)
                     (lambda (string) (setq formatted-prompt string)))
                    ((symbol-function 'vterm-send-return) #'ignore)
                    ((symbol-function 'vterm-clear) #'ignore)
                    ((symbol-function 'sit-for) #'ignore))
            (ecc-run test-prompt test-content)
            (should (string= formatted-prompt
                             (format ecc-prompt-template
                                     (concat test-prompt "\n\n"
                                             test-content))))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer))))

(ert-deftest test-ecc-run-on-region-extracts-region-content
    ()
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (test-buffer (generate-new-buffer "*TEST-REGION*"))
        (prompt-used nil)
        (content-used nil))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)
          (with-current-buffer test-buffer
            (insert "Test content for region")
            (cl-letf (((symbol-function 'ecc-run)
                       (lambda (prompt content)
                         (setq prompt-used prompt
                               content-used content))))
              (ecc-run-on-region 1 10 "Test prompt")
              (should (string= prompt-used "Test prompt"))
              (should (string= content-used "Test cont")))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer orig-buffer))))

(ert-deftest test-ecc-run-on-buffer-extracts-buffer-content
    ()
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (test-buffer (generate-new-buffer "*TEST-BUFFER*"))
        (prompt-used nil)
        (content-used nil))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)
          (with-current-buffer test-buffer
            (insert "Test buffer content")
            (cl-letf (((symbol-function 'ecc-run)
                       (lambda (prompt content)
                         (setq prompt-used prompt
                               content-used content))))
              (ecc-run-on-buffer "Test prompt")
              (should (string-match-p "Test prompt" prompt-used))
              (should
               (string-match-p
                (regexp-quote (buffer-name test-buffer)) prompt-used))
              (should (string= content-used "Test buffer content")))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (setq ecc-buffer orig-buffer))))


(provide 'test-ecc-run)

(when
    (not load-file-name)
  (message "test-ecc-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))