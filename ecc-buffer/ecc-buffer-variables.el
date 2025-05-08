;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 00:15:09>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-buffer/ecc-buffer-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-variables)

;; Definitions
;; ------------------------------

(defvar ecc-buffer-registered-buffers-alist
  nil
  "Alist of (buffer . state) for registered Claude buffers.")

(defvar ecc-state-available-states
  '(nil ready waiting y/n y/y/n)
  "List of available buffer states for Claude buffers.")

(defvar ecc-buffer-current-buffer nil
  "The current Claude buffer being used.")

(defvar ecc-buffer-property-defaults
  '((role . "assistant")
    (project . nil))
  "Default properties for Claude buffers.")


(provide 'ecc-buffer-variables)

(when
    (not load-file-name)
  (message "ecc-buffer-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))