;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: root
;;; Timestamp: <2025-05-04 23:49:31>
;;; File: /root/.emacs.d/lisp/emacs-claude-code-auto-accept/emacs-claude-auto-accept-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defgroup emacs-claude nil
  "Customization group for emacs-claude.auto-accept package."
  :group 'external)

(defcustom emacs-claude-buffer-name "*CLAUDE-CODE*"
  "Buffer name where Claude prompts appear."
  :type 'string
  :group 'emacs-claude)

(defvar emacs-claude-auto-accept-timer nil
  "Timer object for polling Claude prompts.")

(defcustom emacs-claude-prompt-y/n "❯ 1. Yes"
  "Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-prompt-y/y/n "❯ 2. Yes, and"
  "y/Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-prompt-waiting
  "│ >                                                                                                  │"
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-prompt-initial-waiting
  "│ > Try "
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-auto-accept-interval-sec 1
  "Interval in seconds between checks for Claude prompts."
  :type 'string
  :group 'emacs-claude)

(provide 'emacs-claude-auto-accept-variables)

(when
    (not load-file-name)
  (message "emacs-claude-auto-accept-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))