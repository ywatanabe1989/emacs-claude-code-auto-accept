;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:25>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-buffer-registry.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-buffer-registry)
(require 'ecc-variables)
(require 'ecc-update-mode-line)

(ert-deftest test-ecc-buffer-registry-loadable ()
  "Test that ecc-buffer-registry loads properly."
  (should (featurep 'ecc-buffer-registry)))

(ert-deftest test-ecc-buffer-timestamps-defined ()
  "Test that ecc-buffer-timestamps is defined."
  (should (boundp 'ecc-buffer-timestamps))
  (should (hash-table-p ecc-buffer-timestamps)))

(ert-deftest test-ecc-buffer-registry-cleanup-buffers ()
  "Test that cleanup removes dead buffers."
  (let ((original-buffers ecc-buffers)
        (original-active ecc-active-buffer)
        (test-buffer (generate-new-buffer "*test-claude*")))
    ;; Setup test environment
    (setq ecc-buffers (list test-buffer))
    (setq ecc-active-buffer test-buffer)
    
    ;; Kill the buffer and run cleanup
    (kill-buffer test-buffer)
    (ecc-buffer-registry-cleanup-buffers)
    
    ;; Verify cleanup worked
    (should (equal ecc-buffers '()))
    (should (eq ecc-active-buffer nil))
    
    ;; Restore original state
    (setq ecc-buffers original-buffers)
    (setq ecc-active-buffer original-active)))

(ert-deftest test-ecc-buffer-unregister-buffer ()
  "Test buffer unregistration functionality."
  (let ((original-buffers ecc-buffers)
        (original-active ecc-active-buffer)
        (test-buffer (generate-new-buffer "*test-claude*")))
    ;; Setup test environment
    (setq ecc-buffers (list test-buffer))
    (setq ecc-active-buffer test-buffer)
    (puthash test-buffer (current-time) ecc-buffer-timestamps)
    
    ;; Test unregistration
    (cl-letf (((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore))
      (ecc-buffer-unregister-buffer test-buffer))
    
    ;; Verify unregistration worked
    (should (equal ecc-buffers '()))
    (should (eq ecc-active-buffer nil))
    
    ;; Clean up
    (kill-buffer test-buffer)
    
    ;; Restore original state
    (setq ecc-buffers original-buffers)
    (setq ecc-active-buffer original-active)))

(ert-deftest test-ecc-buffer-switch-to-active-buffer ()
  "Test switching active buffer."
  (let ((original-buffers ecc-buffers)
        (original-active ecc-active-buffer)
        (test-buffer1 (generate-new-buffer "*test-claude-1*"))
        (test-buffer2 (generate-new-buffer "*test-claude-2*")))
    ;; Setup test environment
    (setq ecc-buffers (list test-buffer1 test-buffer2))
    (setq ecc-active-buffer test-buffer1)
    (puthash test-buffer1 (current-time) ecc-buffer-timestamps)
    (puthash test-buffer2 (current-time) ecc-buffer-timestamps)
    
    ;; Test switching active buffer
    (cl-letf (((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore))
      (ecc-buffer-switch-to-active-buffer test-buffer2))
    
    ;; Verify switch worked
    (should (eq ecc-active-buffer test-buffer2))
    
    ;; Clean up
    (kill-buffer test-buffer1)
    (kill-buffer test-buffer2)
    
    ;; Restore original state
    (setq ecc-buffers original-buffers)
    (setq ecc-active-buffer original-active)))

(ert-deftest test-ecc-buffer-list-buffers ()
  "Test listing buffers function."
  (let ((original-buffers ecc-buffers)
        (original-active ecc-active-buffer)
        (test-buffer1 (generate-new-buffer "*test-claude-1*"))
        (test-buffer2 (generate-new-buffer "*test-claude-2*")))
    ;; Setup test environment
    (setq ecc-buffers (list test-buffer1 test-buffer2))
    (setq ecc-active-buffer test-buffer1)
    
    ;; Test listing buffers - should not error
    (should-not (with-temp-buffer 
                  (let ((standard-output (current-buffer)))
                    (ecc-buffer-list-buffers)
                    nil)))
    
    ;; Clean up
    (kill-buffer test-buffer1)
    (kill-buffer test-buffer2)
    
    ;; Restore original state
    (setq ecc-buffers original-buffers)
    (setq ecc-active-buffer original-active)))

(provide 'test-ecc-buffer-registry)

(when (not load-file-name)
  (message "test-ecc-buffer-registry.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))