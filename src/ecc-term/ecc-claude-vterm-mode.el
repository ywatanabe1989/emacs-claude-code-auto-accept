;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 02:15:24>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-term/ecc-claude-vterm-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; Required dependencies
(require 'ecc-variables)
(require 'ecc-state/ecc-state)
(require 'ecc-state/ecc-state-detect)
(require 'ecc-buffer/ecc-buffer-registry)

;; Attempt to load vterm if available
(defvar ecc-claude-vterm--vterm-available
  (condition-case nil
      (progn (require 'vterm) t)
    (error nil))
  "Whether the vterm package is available.")

;; Customization group
(defgroup ecc-claude-vterm nil
  "Optimized vterm mode for Claude interaction."
  :group 'ecc
  :prefix "ecc-claude-vterm-")

;; Custom variables
(defcustom ecc-claude-vterm-line-numbers nil
  "Whether to display line numbers in Claude vterm buffers.
Disabling line numbers can improve performance for large outputs."
  :type 'boolean
  :group 'ecc-claude-vterm)

(defcustom ecc-claude-vterm-scroll-conservatively 10000
  "Value for scroll-conservatively in Claude vterm buffers.
Higher values prevent recentering during fast output."
  :type 'integer
  :group 'ecc-claude-vterm)

(defcustom ecc-claude-vterm-truncate-lines t
  "Whether to truncate lines in Claude vterm buffers.
Enabling truncation can improve performance for long lines."
  :type 'boolean
  :group 'ecc-claude-vterm)

;; Define a parent mode to fallback to if vterm isn't available
(defvar ecc-claude-vterm-parent-mode
  (if ecc-claude-vterm--vterm-available
      'vterm-mode
    'special-mode)
  "Parent mode for ecc-claude-vterm-mode.")

;; Mode menu
(defvar ecc-claude-vterm-menu
  (let ((menu (make-sparse-keymap "Claude-VTerm")))
    (define-key menu [ecc-claude-vterm-clear]
      '(menu-item "Clear buffer" ecc-claude-vterm-clear
                  :help "Clear the vterm buffer"))
    (define-key menu [ecc-claude-vterm-separator-1]
      '(menu-item "--"))
    (define-key menu [ecc-claude-vterm-retry]
      '(menu-item "Retry (r)" ecc-claude-vterm-retry
                  :help "Send 'r' to retry current operation"))
    (define-key menu [ecc-claude-vterm-no]
      '(menu-item "No (n)" ecc-claude-vterm-no
                  :help "Send 'n' to respond negatively"))
    (define-key menu [ecc-claude-vterm-yes]
      '(menu-item "Yes (y)" ecc-claude-vterm-yes
                  :help "Send 'y' to respond affirmatively"))
    (define-key menu [ecc-claude-vterm-separator-2]
      '(menu-item "--"))
    (define-key menu [ecc-claude-vterm-interrupt]
      '(menu-item "Interrupt" ecc-claude-vterm-interrupt
                  :help "Interrupt the current Claude process"))
    menu)
  "Menu for Claude-VTerm mode.")

;; Keybindings for Claude VTERM mode
(defvar ecc-claude-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; If vterm is available, inherit its keymap
    (when ecc-claude-vterm--vterm-available
      (set-keymap-parent map vterm-mode-map))
    
    ;; Claude-specific keybindings
    (define-key map (kbd "C-c C-k") 'ecc-claude-vterm-interrupt)
    (define-key map (kbd "C-c C-c") 'ecc-claude-vterm-interrupt)
    (define-key map (kbd "C-c C-y") 'ecc-claude-vterm-yes)
    (define-key map (kbd "C-c C-n") 'ecc-claude-vterm-no)
    (define-key map (kbd "C-c C-r") 'ecc-claude-vterm-retry)
    (define-key map (kbd "C-c C-l") 'ecc-claude-vterm-clear)
    
    ;; Add menu
    (define-key map [menu-bar claude-vterm] (cons "Claude" ecc-claude-vterm-menu))
    
    map)
  "Keymap for `ecc-claude-vterm-mode'.")

;; Mode definition
(define-derived-mode ecc-claude-vterm-mode ecc-claude-vterm-parent-mode "Claude-VTerm"
  "Major mode for optimized Claude interaction in vterm.
This mode is optimized for high-performance streaming output.

Key bindings:
\\{ecc-claude-vterm-mode-map}"
  ;; Disable line numbers
  (when (and (boundp 'display-line-numbers-mode)
             (not ecc-claude-vterm-line-numbers))
    (display-line-numbers-mode -1))
  
  ;; Performance optimizations
  (setq-local scroll-conservatively ecc-claude-vterm-scroll-conservatively
              scroll-margin 0
              scroll-step 1
              fast-but-imprecise-scrolling t
              truncate-lines ecc-claude-vterm-truncate-lines)
  
  ;; Visual indicators for Claude state
  (ecc-claude-vterm-setup-mode-line)
  
  ;; Register in buffer registry
  (ecc-buffer-register-buffer (current-buffer)))

;; Mode-line indicator for Claude state
(defun ecc-claude-vterm-setup-mode-line ()
  "Set up mode line indicator for Claude state."
  (setq mode-line-process 
        '(:eval (ecc-claude-vterm-mode-line-state-indicator))))

(defun ecc-claude-vterm-mode-line-state-indicator ()
  "Return mode line indicator for current Claude state."
  (let ((state (ecc-state-get)))
    (cond
     ((eq state 'waiting) " [Waiting]")
     ((eq state 'y/n) " [Y/N]")
     ((eq state 'y/y/n) " [Y/Y/N]")
     (t ""))))

;; Claude interaction functions
(defun ecc-claude-vterm-interrupt ()
  "Interrupt the current Claude process."
  (interactive)
  (if ecc-claude-vterm--vterm-available
      (vterm-send-string "\C-c")
    (message "Interrupt not available without vterm")))

(defun ecc-claude-vterm-yes ()
  "Send 'y' response to Claude prompt."
  (interactive)
  (if ecc-claude-vterm--vterm-available
      (progn
        (vterm-send-string "y")
        (vterm-send-return))
    (message "Yes response not available without vterm")))

(defun ecc-claude-vterm-no ()
  "Send 'n' response to Claude prompt."
  (interactive)
  (if ecc-claude-vterm--vterm-available
      (progn
        (vterm-send-string "n")
        (vterm-send-return))
    (message "No response not available without vterm")))

(defun ecc-claude-vterm-retry ()
  "Send 'r' response to Claude prompt for retry."
  (interactive)
  (if ecc-claude-vterm--vterm-available
      (progn
        (vterm-send-string "r")
        (vterm-send-return))
    (message "Retry response not available without vterm")))

(defun ecc-claude-vterm-clear ()
  "Clear the vterm buffer."
  (interactive)
  (if ecc-claude-vterm--vterm-available
      (vterm-clear)
    (erase-buffer)))

;; Function to create new Claude vterm buffer
(defun ecc-claude-vterm ()
  "Create a new Claude vterm buffer with optimized settings."
  (interactive)
  (unless ecc-claude-vterm--vterm-available
    (error "Cannot create Claude vterm buffer: vterm package not available"))
  
  (let* ((buffer-name (format "*CLAUDE-VTERM-%02d*"
                              (+ (length
                                  (ecc-buffer-get-registered-buffers))
                                 1)))
         (new-buffer (get-buffer-create buffer-name)))
    (with-current-buffer new-buffer
      (ecc-claude-vterm-mode)
      (set (make-local-variable 'ecc-original-name) buffer-name))
    (switch-to-buffer new-buffer)
    new-buffer))

;; Provide backward compatibility for existing code that uses vterm
(unless ecc-claude-vterm--vterm-available
  ;; Stub functions for vterm when not available
  (defvar vterm-max-scrollback 1000
    "Stub variable for vterm-max-scrollback.")
  
  (defvar vterm-buffer-name "*vterm*"
    "Stub variable for vterm-buffer-name.")
  
  (defun vterm--internal (arg)
    "Stub function for vterm--internal."
    (error "vterm not available")))

(provide 'ecc-term/ecc-claude-vterm-mode)

(when (not load-file-name)
  (message "ecc-claude-vterm-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))