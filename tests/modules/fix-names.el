;;; fix-names.el --- Fix function and variable name compatibility for tests

;; This file provides compatibility aliases for functions and variables that have been
;; renamed or moved between modules.

;; Require all buffer modules
(require 'ecc-buffer/ecc-buffer-variables)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-buffer/ecc-buffer-verification)
(require 'ecc-buffer/ecc-buffer-current)
(require 'ecc-buffer/ecc-buffer-state)
(require 'ecc-buffer/ecc-buffer-stale)
(require 'ecc-buffer/ecc-buffer-navigation)
(require 'ecc-buffer/ecc-buffer-timestamp)
(require 'ecc-buffer/ecc-buffer)

;; Require state modules
(require 'ecc-state/ecc-state)
(require 'ecc-state/ecc-state-detect)

;; Require other modules
(require 'ecc-variables)
(require 'ecc-auto)
(require 'ecc-send)
(require 'ecc-update-mode-line)

;; Buffer registry functions
(defvar ecc-buffers nil)
(defun ecc-buffer-registry-cleanup-buffers ()
  "Compatibility function for ecc-buffer-cleanup-buffer-registry."
  (when (fboundp 'ecc-buffer-cleanup-buffer-registry)
    (ecc-buffer-cleanup-buffer-registry)))

(defun ecc-buffer-get-all-buffers ()
  "Compatibility function for ecc-buffer-get-registered-buffers."
  (when (fboundp 'ecc-buffer-get-registered-buffers)
    (ecc-buffer-get-registered-buffers)))

;; Buffer current functions
(defun ecc-buffer-current-get-buffer ()
  "Compatibility function for getting current active buffer."
  ecc-buffer-current-buffer)

;; Buffer renaming
(defun ecc-buffer-rename-buffer (active)
  "Compatibility function for buffer renaming."
  (when (fboundp 'ecc-update-mode-line) 
    (ecc-update-mode-line active)))

;; Auto functions
(defun --ecc-auto-check-and-restart ()
  "Compatibility function for ecc-auto-check-and-restart."
  (when (fboundp 'ecc-auto-check-and-restart)
    (ecc-auto-check-and-restart)))

;; Buffer registration
(defun --ecc-buffer-register-buffer (buffer)
  "Compatibility function for buffer registration."
  (when (fboundp 'ecc-buffer-register-buffer)
    (ecc-buffer-register-buffer buffer)))

;; Activate these variables that aren't declared in the current code
(defvar ecc-buffer-registered-buffers (make-hash-table))
(defvar ecc-buffer-current-active-buffer nil)
(defvar --ecc-send-string-calls nil)

;; Make ecc-buffer-registered-buffers point to ecc-buffer-registered-buffers-alist
(setq ecc-buffer-registered-buffers ecc-buffer-registered-buffers-alist)
(setq ecc-buffer-current-active-buffer ecc-buffer-current-buffer)

(provide 'fix-names)