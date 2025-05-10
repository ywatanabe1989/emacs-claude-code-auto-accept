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

;; 1. Module loading tests
(ert-deftest test-ecc-send-loadable ()
  "Test that ecc-send module loads properly."
  (should (featurep 'ecc-send)))

;; 2. Function existence tests
(ert-deftest test-ecc-send-accept-defined ()
  "Test that ecc-send-accept function is defined."
  (should (fboundp 'ecc-send-accept)))

(ert-deftest test-ecc-auto-send-1-defined ()
  "Test that --ecc-auto-send-1-y/n function is defined."
  (should (fboundp '--ecc-auto-send-1-y/n)))

(ert-deftest test-ecc-auto-send-2-defined ()
  "Test that --ecc-auto-send-2-y/y/n function is defined."
  (should (fboundp '--ecc-auto-send-2-y/y/n)))

(ert-deftest test-ecc-auto-send-continue-defined ()
  "Test that --ecc-auto-send-continue-on-y/y/n function is defined."
  (should (fboundp '--ecc-auto-send-continue-on-y/y/n)))

;; 3. Mock functionality tests
(ert-deftest test-vterm-mock-reset ()
  "Test that vterm-mock-reset clears mock variables."
  (setq vterm-mock-last-string "test"
        vterm-mock-return-count 5)
  (vterm-mock-reset)
  (should (eq vterm-mock-last-string nil))
  (should (eq vterm-mock-return-count 0)))

(ert-deftest test-vterm-send-string-mock ()
  "Test that vterm-send-string mock captures the string."
  (vterm-mock-reset)
  (vterm-send-string "test-string")
  (should (string= vterm-mock-last-string "test-string")))

(ert-deftest test-vterm-send-return-mock ()
  "Test that vterm-send-return mock increments the counter."
  (vterm-mock-reset)
  (vterm-send-return)
  (should (eq vterm-mock-return-count 1))
  (vterm-send-return)
  (should (eq vterm-mock-return-count 2)))

;; 4. Basic response tests
(ert-deftest test-ecc-send-option-1 ()
  "Test that '1' is properly sent via vterm-send-string."
  (vterm-mock-reset)
  (vterm-send-string "1")
  (should (string= vterm-mock-last-string "1")))

(ert-deftest test-ecc-send-option-2 ()
  "Test that '2' is properly sent via vterm-send-string."
  (vterm-mock-reset)
  (vterm-send-string "2")
  (should (string= vterm-mock-last-string "2")))

(ert-deftest test-ecc-send-continue ()
  "Test that 'continue' is properly sent via vterm-send-string."
  (vterm-mock-reset)
  (vterm-send-string "continue")
  (should (string= vterm-mock-last-string "continue")))

;; 5. Handler routing tests
(ert-deftest test-ecc-send-routes-to-y-n-handler ()
  "Test that send-accept calls the y/n handler when y/n state is detected."
  (let ((handled-by nil))
    (cl-letf
        (((symbol-function '--ecc-state-y/y/n-p) (lambda () nil))
         ((symbol-function '--ecc-state-y/n-p) (lambda () t))
         ((symbol-function '--ecc-state-waiting-p) (lambda () nil))
         ((symbol-function '--ecc-state-initial-waiting-p) (lambda () nil))
         ((symbol-function '--ecc-auto-send-1-y/n) (lambda () (setq handled-by 'y-n)))
         ((symbol-function '--ecc-auto-send-2-y/y/n) (lambda () (setq handled-by 'y-y-n)))
         ((symbol-function '--ecc-auto-send-continue-on-y/y/n) (lambda () (setq handled-by 'continue)))
         ((symbol-function 'vterm-clear) #'ignore))
      
      (ecc-send-accept)
      (should (eq handled-by 'y-n)))))

(ert-deftest test-ecc-send-routes-to-y-y-n-handler ()
  "Test that send-accept calls the y/y/n handler when y/y/n state is detected."
  (let ((handled-by nil))
    (cl-letf
        (((symbol-function '--ecc-state-y/y/n-p) (lambda () t))
         ((symbol-function '--ecc-state-y/n-p) (lambda () nil))
         ((symbol-function '--ecc-state-waiting-p) (lambda () nil))
         ((symbol-function '--ecc-state-initial-waiting-p) (lambda () nil))
         ((symbol-function '--ecc-auto-send-1-y/n) (lambda () (setq handled-by 'y-n)))
         ((symbol-function '--ecc-auto-send-2-y/y/n) (lambda () (setq handled-by 'y-y-n)))
         ((symbol-function '--ecc-auto-send-continue-on-y/y/n) (lambda () (setq handled-by 'continue)))
         ((symbol-function 'vterm-clear) #'ignore))
      
      (ecc-send-accept)
      (should (eq handled-by 'y-y-n)))))

(ert-deftest test-ecc-send-routes-to-waiting-handler ()
  "Test that send-accept calls the waiting handler when waiting state is detected."
  (let ((handled-by nil))
    (cl-letf
        (((symbol-function '--ecc-state-y/y/n-p) (lambda () nil))
         ((symbol-function '--ecc-state-y/n-p) (lambda () nil))
         ((symbol-function '--ecc-state-waiting-p) (lambda () t))
         ((symbol-function '--ecc-state-initial-waiting-p) (lambda () nil))
         ((symbol-function '--ecc-auto-send-1-y/n) (lambda () (setq handled-by 'y-n)))
         ((symbol-function '--ecc-auto-send-2-y/y/n) (lambda () (setq handled-by 'y-y-n)))
         ((symbol-function '--ecc-auto-send-continue-on-y/y/n) (lambda () (setq handled-by 'continue)))
         ((symbol-function 'vterm-clear) #'ignore))
      
      (ecc-send-accept)
      (should (eq handled-by 'continue)))))

(ert-deftest test-ecc-send-routes-to-initial-waiting-handler ()
  "Test that send-accept calls waiting handler when initial waiting state is detected."
  (let ((handled-by nil))
    (cl-letf
        (((symbol-function '--ecc-state-y/y/n-p) (lambda () nil))
         ((symbol-function '--ecc-state-y/n-p) (lambda () nil))
         ((symbol-function '--ecc-state-waiting-p) (lambda () nil))
         ((symbol-function '--ecc-state-initial-waiting-p) (lambda () t))
         ((symbol-function '--ecc-auto-send-1-y/n) (lambda () (setq handled-by 'y-n)))
         ((symbol-function '--ecc-auto-send-2-y/y/n) (lambda () (setq handled-by 'y-y-n)))
         ((symbol-function '--ecc-auto-send-continue-on-y/y/n) (lambda () (setq handled-by 'continue)))
         ((symbol-function 'vterm-clear) #'ignore))
      
      (ecc-send-accept)
      (should (eq handled-by 'continue)))))

;; 6. Mock buffer behavior tests
(ert-deftest test-ecc-buffer-setup-teardown ()
  "Test that buffer setup and teardown works properly."
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-TEST*")))
    (unwind-protect
        (progn 
          (setq ecc-buffer mock-buffer)
          (should (buffer-live-p ecc-buffer))
          (should (eq ecc-buffer mock-buffer)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer))))

(provide 'test-ecc-send)

(when
    (not load-file-name)
  (message "test-ecc-send.el loaded. %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))