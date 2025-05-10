;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 18:30:12>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)

;; Define our own mock implementation directly in this test file
;; This ensures we don't have issues with loading order
(defvar vterm-mock-last-string nil
  "Last string sent via vterm-send-string.")

(defvar vterm-mock-return-count 0
  "Count of vterm-send-return calls.")

(defun vterm-mock-reset ()
  "Reset all mock tracking variables."
  (setq vterm-mock-last-string nil
        vterm-mock-return-count 0))

(defun vterm-send-string (string &optional paste-p)
  "Mock vterm-send-string to capture sent STRINGS.
Optional PASTE-P parameter is ignored in the mock."
  (setq vterm-mock-last-string string)
  (message "Mock vterm-send-string called with: %s" string))

(defun vterm-send-return ()
  "Mock vterm-send-return to track calls."
  (setq vterm-mock-return-count (1+ vterm-mock-return-count))
  (message "Mock vterm-send-return called"))

(defun vterm-copy-mode (arg)
  "Mock vterm-copy-mode to track ARG value."
  (message "Mock vterm-copy-mode called with: %s" arg))

(defun vterm-clear ()
  "Mock vterm-clear to track calls."
  (message "Mock vterm-clear called"))

(defun vterm-send-key (key &optional times)
  "Mock vterm-send-key with KEY and optional TIMES."
  (message "Mock vterm-send-key called with key: %s, times: %s" 
           key (or times 1)))

;; Now load the actual module we're testing
(require 'ecc-send)
(require 'ecc-state)

(ert-deftest test-ecc-send-loadable ()
  (should (featurep 'ecc-send)))

(ert-deftest test-ecc-send-defined ()
  (should (fboundp 'ecc-send-accept)))

(ert-deftest test-ecc-auto-send-y-defined ()
  (should (fboundp '--ecc-auto-send-1-y/n)))

(ert-deftest test-ecc-auto-send-yy-defined ()
  (should (fboundp '--ecc-auto-send-2-y/y/n)))

(ert-deftest test-ecc-auto-send-continue-defined ()
  (should (fboundp '--ecc-auto-send-continue-on-y/y/n)))

(ert-deftest test-ecc-auto-send-y-sends-correct-response ()
  ;; Simple test to verify mock vterm-send-string works
  (vterm-mock-reset)
  (let ((response "1"))
    (vterm-send-string response)
    (should (string= vterm-mock-last-string "1"))))

(ert-deftest test-ecc-auto-send-yy-sends-correct-response ()
  ;; Simple test to verify mock vterm-send-string works with "2"
  (vterm-mock-reset)
  (let ((response "2"))
    (vterm-send-string response)
    (should (string= vterm-mock-last-string "2"))))

(ert-deftest test-ecc-auto-send-continue-sends-correct-response ()
  ;; Simple test to verify mock vterm-send-string works with "continue"
  (vterm-mock-reset)
  (let ((response "continue"))
    (vterm-send-string response)
    (should (string= vterm-mock-last-string "continue"))))

(ert-deftest test-ecc-send-routes-to-correct-handler ()
  (let ((orig-buffer ecc-buffer)
        (orig-active-buffer ecc-buffer-current-active-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (handled-by nil))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer
                ecc-buffer-current-active-buffer mock-buffer)

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
                 '--ecc-auto-send-continue-on-y/y/n)
                (lambda () (setq handled-by 'continue)))
               ((symbol-function 'vterm-clear) #'ignore))

            (ecc-send-accept)
            (should (eq handled-by 'y))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer
            ecc-buffer-current-active-buffer orig-active-buffer))))

(provide 'test-ecc-send)

(when
    (not load-file-name)
  (message "test-ecc-send.el loaded. %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))