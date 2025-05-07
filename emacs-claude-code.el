;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:38>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(let ((this-dir
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir))

(require 'ecc-variables)
(require 'ecc-state)
(require 'ecc-send)
(require 'ecc-update-mode-line)
(require 'ecc-auto)
(require 'ecc-run)
(require 'ecc-repository)
(require 'ecc-dired)
(require 'ecc-buffer-registry)
(require 'ecc-buffer-navigation)
(require 'ecc-bindings)


(provide 'emacs-claude-code)

(when
    (not load-file-name)
  (message "emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))