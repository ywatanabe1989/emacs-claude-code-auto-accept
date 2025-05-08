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

;; Load module umbrella files
(require 'ecc-variables)
(require 'ecc-buffer)
(require 'ecc-state-detect)
(require 'ecc-state)
(require 'ecc-send)
;; (require 'ecc-agent)
(require 'ecc-update-mode-line)
(require 'ecc-auto)
(require 'ecc-run)
(require 'ecc-repository)
(require 'ecc-repository-view)
(require 'ecc-dired)
(require 'ecc-history)
(require 'ecc-large-buffer)
(require 'ecc-bindings)
(require 'ecc-mode)
(require 'ecc-template-mode)

;; Enable modes if ecc-auto-enable is set
(when (and (boundp 'ecc-auto-enable) ecc-auto-enable)
  (ecc-mode 1))


(provide 'emacs-claude-code)

(when
    (not load-file-name)
  (message "emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))