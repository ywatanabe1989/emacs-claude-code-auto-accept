;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 13:47:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-bindings.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-buffer-navigation)
(require 'ecc-auto)
(require 'ecc-run)
(require 'ecc-send)
(require 'ecc-repository)
(require 'ecc-templates)

(defgroup ecc-keymaps nil
  "Key bindings for emacs-claude-code."
  :group 'emacs-claude)

(defcustom ecc-keymap-prefix "C-c c"
  "Prefix for ecc-mode key bindings."
  :type 'string
  :group 'ecc-keymaps)

(defvar ecc-keymap
  (let ((map (make-sparse-keymap)))
    ;; Buffer navigation
    (define-key map (kbd "c") 'ecc-buffer-create)
    (define-key map (kbd "n") 'ecc-buffer-next)
    (define-key map (kbd "p") 'ecc-buffer-prev)
    (define-key map (kbd "l") 'ecc-buffer-list-buffers)
    ;; Auto-accept functions
    (define-key map (kbd "a") 'ecc-auto-toggle)
    (define-key map (kbd "y") 'ecc-send-accept)
    ;; Run functions
    (define-key map (kbd "r") 'ecc-run-on-region)
    (define-key map (kbd "b") 'ecc-run-on-buffer)
    (define-key map (kbd "q") 'ecc-run-quick)
    ;; Send functions
    (define-key map (kbd "i") 'ecc-send-init)
    (define-key map (kbd "ESC") 'ecc-send-interrupt)
    (define-key map (kbd "t") 'ecc-send-template)
    ;; History/Memory management functions
    (define-key map (kbd "m") 'ecc-send-edit-memory)
    (define-key map (kbd "h c") 'ecc-send-compact-history)
    (define-key map (kbd "h l") 'ecc-send-clear-history)
    ;; Repository functions
    (define-key map (kbd "R") 'ecc-repository-copy-contents)
    ;; Template functions
    (define-key map (kbd "T c") 'ecc-template-create)
    (define-key map (kbd "T e") 'ecc-template-edit)
    map)
  "Keymap for ECC commands.")

(defun ecc-reload-keymap ()
  "Reload ECC keymap with updated bindings."
  (interactive)
  (setq ecc-keymap
        (let ((map (make-sparse-keymap)))
          ;; Buffer navigation
          (define-key map (kbd "c") 'ecc-buffer-create)
          (define-key map (kbd "n") 'ecc-buffer-next)
          (define-key map (kbd "p") 'ecc-buffer-prev)
          (define-key map (kbd "l") 'ecc-buffer-list-buffers)
          ;; Auto-accept functions
          (define-key map (kbd "a") 'ecc-auto-toggle)
          (define-key map (kbd "y") 'ecc-send-accept)
          ;; Run functions
          (define-key map (kbd "r") 'ecc-run-on-region)
          (define-key map (kbd "b") 'ecc-run-on-buffer)
          (define-key map (kbd "q") 'ecc-run-quick)
          ;; Send functions
          (define-key map (kbd "i") 'ecc-send-init)
          (define-key map (kbd "ESC") 'ecc-send-interrupt)
          (define-key map (kbd "t") 'ecc-send-template)
          ;; History/Memory management functions
          (define-key map (kbd "m") 'ecc-send-edit-memory)
          (define-key map (kbd "h c") 'ecc-send-compact-history)
          (define-key map (kbd "h l") 'ecc-send-clear-history)
          ;; Repository functions
          (define-key map (kbd "R") 'ecc-repository-copy-contents)
          ;; Template functions
          (define-key map (kbd "T c") 'ecc-template-create)
          (define-key map (kbd "T e") 'ecc-template-edit)
          map))
  (ecc-setup-keymap)
  (message "ECC keymap reloaded"))

 (defun ecc-setup-keymap ()
   "Set up the keymap for ECC commands."
   (let ((prefix-key (kbd ecc-keymap-prefix)))
     ;; Global map
     (global-set-key prefix-key ecc-keymap)
     ;; Apply same keymap to dired mode
     (with-eval-after-load 'dired
       (define-key dired-mode-map prefix-key ecc-keymap))

;; Set up the keymap when this file is loaded
(ecc-reload-keymap)


(provide 'ecc-bindings)

(when
    (not load-file-name)
  (message "ecc-bindings.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
