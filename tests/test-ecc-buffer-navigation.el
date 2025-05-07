;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:21>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-buffer-navigation.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-buffer-navigation)
(require 'ecc-variables)
(require 'ecc-update-mode-line)

(ert-deftest test-ecc-buffer-navigation-loadable ()
  "Test that ecc-buffer-navigation loads properly."
  (should (featurep 'ecc-buffer-navigation)))

(ert-deftest test-ecc-buffer-create-function-exists ()
  "Test that ecc-buffer-create function exists."
  (should (fboundp 'ecc-buffer-create)))

(ert-deftest test-ecc-buffer-next-function-exists ()
  "Test that ecc-buffer-next function exists."
  (should (fboundp 'ecc-buffer-next)))

(ert-deftest test-ecc-buffer-prev-function-exists ()
  "Test that ecc-buffer-prev function exists."
  (should (fboundp 'ecc-buffer-prev)))

(ert-deftest test-ecc-buffer-next-prev-with-empty-buffers ()
  "Test next/prev functions with empty buffer list."
  (let ((original-buffers ecc-buffers)
        (original-active ecc-active-buffer))
    ;; Setup empty environment
    (setq ecc-buffers nil)
    (setq ecc-active-buffer nil)
    
    ;; Test functions - should not error with empty list
    (should-not (ecc-buffer-next))
    (should-not (ecc-buffer-prev))
    
    ;; Restore original state
    (setq ecc-buffers original-buffers)
    (setq ecc-active-buffer original-active)))

(ert-deftest test-ecc-buffer-next-prev-navigation ()
  "Test buffer navigation between multiple buffers."
  (let ((original-buffers ecc-buffers)
        (original-active ecc-active-buffer)
        (test-buffer1 (generate-new-buffer "*test-claude-1*"))
        (test-buffer2 (generate-new-buffer "*test-claude-2*"))
        (test-buffer3 (generate-new-buffer "*test-claude-3*")))
    
    ;; Setup test environment with timestamps in order
    (setq ecc-buffers (list test-buffer1 test-buffer2 test-buffer3))
    (setq ecc-active-buffer test-buffer1)
    
    ;; Set timestamps with delays to ensure proper ordering
    (puthash test-buffer1 (current-time) ecc-buffer-timestamps)
    (sleep-for 0.01)
    (puthash test-buffer2 (current-time) ecc-buffer-timestamps)
    (sleep-for 0.01)
    (puthash test-buffer3 (current-time) ecc-buffer-timestamps)
    
    ;; Mock display-buffer function
    (cl-letf (((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore))
      
      ;; Test next navigation
      (ecc-buffer-next)
      (should (eq ecc-active-buffer test-buffer2))
      
      (ecc-buffer-next)
      (should (eq ecc-active-buffer test-buffer3))
      
      ;; Test cycling to beginning
      (ecc-buffer-next)
      (should (eq ecc-active-buffer test-buffer1))
      
      ;; Test prev navigation
      (ecc-buffer-prev)
      (should (eq ecc-active-buffer test-buffer3))
      
      (ecc-buffer-prev)
      (should (eq ecc-active-buffer test-buffer2))
      
      ;; Test cycling to end
      (ecc-buffer-prev)
      (should (eq ecc-active-buffer test-buffer1)))
    
    ;; Clean up
    (kill-buffer test-buffer1)
    (kill-buffer test-buffer2)
    (kill-buffer test-buffer3)
    
    ;; Restore original state
    (setq ecc-buffers original-buffers)
    (setq ecc-active-buffer original-active)))

(provide 'test-ecc-buffer-navigation)

(when (not load-file-name)
  (message "test-ecc-buffer-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))