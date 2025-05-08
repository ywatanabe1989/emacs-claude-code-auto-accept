;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 19:55:29>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; emacs-claude-code.el - Main module file

(defun --ecc-add-to-loadpath ()
  "Add all visible (non-hidden) subdirectories to `load-path`."
  (let ((parent (file-name-directory
                 (or load-file-name buffer-file-name))))
    ;; Add parent directory to load-path
    (add-to-list 'load-path parent)
    ;; Add immediate subdirectories to load-path
    (let ((candidates (directory-files parent t "\\`[^.]")))
      (dolist (dir candidates)
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))))

;; Execute loadpath setup
(--ecc-add-to-loadpath)

;; Load core modules
(require 'ecc-variables)
(require 'ecc-update-mode-line)
(require 'ecc-auto)
(require 'ecc-run)
(require 'ecc-send)
(require 'ecc-large-buffer)
(require 'ecc-bindings)
(require 'ecc-mode)

;; Load buffer management modules
(require 'ecc-buffer)

;; Load state management modules 
(require 'ecc-state/ecc-state-detect)
(require 'ecc-state/ecc-state)

;; Load template system modules
(require 'ecc-template/ecc-template-cache)
(require 'ecc-template/ecc-template)
(require 'ecc-template/ecc-template-mode)

;; Load repository integration modules
(require 'ecc-repository)
(require 'ecc-repository-view)
(require 'ecc-dired)

;; Load history and session management
(require 'ecc-history)

;; Enable modes if ecc-auto-enable is set
(when (and (boundp 'ecc-auto-enable) ecc-auto-enable)
  (ecc-mode 1))


(provide 'emacs-claude-code)

(when
    (not load-file-name)
  (message "emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))