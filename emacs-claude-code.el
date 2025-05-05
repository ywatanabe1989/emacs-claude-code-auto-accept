;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:29:54>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(let ((this-dir
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir))

(require 'emacs-claude-code-variables)
(require 'emacs-claude-code-detect-prompt)
(require 'emacs-claude-code-send)
(require 'emacs-claude-code-update-mode-line)
(require 'emacs-claude-code-start-stop)
(require 'emacs-claude-code-run)
(require 'emacs-claude-code-copy-repository)


(provide 'emacs-claude-code)

(when
    (not load-file-name)
  (message "emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))