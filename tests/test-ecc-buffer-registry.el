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

(ert-deftest test-ecc-buffer-cleanup-buffer-registry ()
  "Test that cleanup removes dead buffers."
  (let ((original-buffers ecc-buffer-registered-buffers)
        (original-alist ecc-buffer-registered-buffers-alist)
        (original-active ecc-buffer-current-active-buffer)
        (test-buffer (generate-new-buffer "*test-claude*"))
        (kill-hook-status nil))
    (unwind-protect
        (progn
          ;; Mock kill-buffer-hook to prevent recursion
          (cl-letf (((symbol-function 'ecc-buffer-cleanup-hook) 
                   (lambda () (setq kill-hook-status 'called))))
            ;; Setup test environment
            (setq ecc-buffer-registered-buffers (list test-buffer))
            (setq ecc-buffer-registered-buffers-alist (list (cons test-buffer nil)))
            (setq ecc-buffer-current-active-buffer test-buffer)
            
            ;; Kill the buffer directly without invoking hooks
            (let ((kill-buffer-hook nil)) 
              (kill-buffer test-buffer))
            
            ;; Manually run cleanup
            (ecc-buffer-cleanup-buffer-registry)
            
            ;; Verify cleanup worked
            (should (equal ecc-buffer-registered-buffers-alist nil))))
      
      ;; Restore original state
      (setq ecc-buffer-registered-buffers original-buffers)
      (setq ecc-buffer-registered-buffers-alist original-alist)
      (setq ecc-buffer-current-active-buffer original-active))))

(ert-deftest test-ecc-buffer-unregister-buffer ()
  "Test buffer unregistration functionality."
  (let ((original-buffers ecc-buffer-registered-buffers)
        (original-alist ecc-buffer-registered-buffers-alist)
        (original-active ecc-buffer-current-active-buffer)
        (test-buffer (generate-new-buffer "*test-claude*")))
    ;; Setup test environment
    (setq ecc-buffer-registered-buffers (list test-buffer))
    (setq ecc-buffer-registered-buffers-alist (list (cons test-buffer nil)))
    (setq ecc-buffer-current-active-buffer test-buffer)
    (puthash test-buffer (current-time) ecc-buffer-timestamps)
    
    ;; Test unregistration
    (cl-letf (((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore))
      (ecc-buffer-unregister-buffer test-buffer))
    
    ;; Verify unregistration worked
    (should (equal ecc-buffer-registered-buffers-alist nil))
    
    ;; Clean up
    (kill-buffer test-buffer)
    
    ;; Restore original state
    (setq ecc-buffer-registered-buffers original-buffers)
    (setq ecc-buffer-registered-buffers-alist original-alist)
    (setq ecc-buffer-current-active-buffer original-active)))

(ert-deftest test-ecc-buffer-register-as-active ()
  "Test switching active buffer."
  (let ((original-buffers ecc-buffer-registered-buffers)
        (original-alist ecc-buffer-registered-buffers-alist)
        (original-active ecc-buffer-current-active-buffer)
        (original-current ecc-buffer-current-buffer)
        (test-buffer1 (generate-new-buffer "*test-claude-1*"))
        (test-buffer2 (generate-new-buffer "*test-claude-2*")))
    ;; Setup test environment
    (setq ecc-buffer-registered-buffers (list test-buffer1 test-buffer2))
    (setq ecc-buffer-registered-buffers-alist 
          (list (cons test-buffer1 nil) 
                (cons test-buffer2 nil)))
    (setq ecc-buffer-current-active-buffer test-buffer1)
    (setq ecc-buffer-current-buffer test-buffer1)
    (puthash test-buffer1 (current-time) ecc-buffer-timestamps)
    (puthash test-buffer2 (current-time) ecc-buffer-timestamps)
    
    ;; Test switching active buffer
    (cl-letf (((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore))
      (ecc-buffer-register-as-active test-buffer2))
    
    ;; Verify switch worked
    (should (eq ecc-buffer-current-active-buffer test-buffer2))
    (should (eq ecc-buffer-current-buffer test-buffer2))
    
    ;; Clean up
    (kill-buffer test-buffer1)
    (kill-buffer test-buffer2)
    
    ;; Restore original state
    (setq ecc-buffer-registered-buffers original-buffers)
    (setq ecc-buffer-registered-buffers-alist original-alist)
    (setq ecc-buffer-current-active-buffer original-active)
    (setq ecc-buffer-current-buffer original-current)))

(ert-deftest test-ecc-buffer-list-buffers ()
  "Test listing buffers function."
  (let ((original-buffers ecc-buffer-registered-buffers)
        (original-alist ecc-buffer-registered-buffers-alist)
        (original-active ecc-buffer-current-active-buffer)
        (test-buffer1 (generate-new-buffer "*test-claude-1*"))
        (test-buffer2 (generate-new-buffer "*test-claude-2*")))
    (unwind-protect
        (progn
          ;; Setup test environment
          (setq ecc-buffer-registered-buffers (list test-buffer1 test-buffer2))
          (setq ecc-buffer-registered-buffers-alist 
                (list (cons test-buffer1 nil) 
                      (cons test-buffer2 nil)))
          (setq ecc-buffer-current-active-buffer test-buffer1)
          
          ;; Mock the interactive-p function
          (cl-letf (((symbol-function 'called-interactively-p) 
                   (lambda (&rest _) nil)))
            ;; Test the non-interactive version
            (should (listp (ecc-buffer-list-registered-buffers)))
            ;; Verify list contents
            (let ((result (ecc-buffer-list-registered-buffers)))
              (should (= (length result) 2))
              (should (member (cons test-buffer1 nil) result))
              (should (member (cons test-buffer2 nil) result)))))
      
      ;; Clean up
      (when (buffer-live-p test-buffer1)
        (kill-buffer test-buffer1))
      (when (buffer-live-p test-buffer2)
        (kill-buffer test-buffer2))
      
      ;; Restore original state
      (setq ecc-buffer-registered-buffers original-buffers)
      (setq ecc-buffer-registered-buffers-alist original-alist)
      (setq ecc-buffer-current-active-buffer original-active))))

(provide 'test-ecc-buffer-registry)

(when (not load-file-name)
  (message "test-ecc-buffer-registry.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))