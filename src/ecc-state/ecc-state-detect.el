;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 03:15:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/src/ecc-state/ecc-state-detect.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; This is a compatibility module for ecc-state.el

(require 'ecc-state)

;; Define compatibility functions with proper argument handling
(defun ecc-state-detect-prompt (prompt-text &optional n-lines &rest _)
  "Compatibility function for state detection.
Calls internal state detection with PROMPT-TEXT and optional N-LINES.
Ignores any additional arguments for backward compatibility.
Supports both vterm and standard buffers."
  (--ecc-state-detect-prompt prompt-text n-lines))

;; Provide both features to maintain backward compatibility
(provide 'ecc-state-detect)
(provide 'ecc-state/ecc-state-detect)

(when (not load-file-name)
  (message "ecc-state-detect.el loaded: %s"
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))