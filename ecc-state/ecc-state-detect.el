;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 03:20:04>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/ecc-state-detect.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; This is a compatibility module for ecc-state.el

(provide 'ecc-state/ecc-state-detect)

(when (not load-file-name)
  (message "ecc-state-detect.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))