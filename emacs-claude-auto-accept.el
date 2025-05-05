;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-05 11:17:50>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code-auto-accept/emacs-claude-auto-accept.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(let
    ((this-dir
      (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir))

(require 'emacs-claude-auto-accept-variables)
(require 'emacs-claude-auto-accept-detect-prompt)
(require 'emacs-claude-auto-accept-send)
(require 'emacs-claude-auto-accept-start-stop)


(provide 'emacs-claude-auto-accept)

(when
    (not load-file-name)
  (message "emacs-claude-auto-accept.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))