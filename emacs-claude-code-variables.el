;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 04:08:11>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defgroup emacs-claude nil
  "Customization group for emacs-claude package."
  :group 'external)

(defcustom emacs-claude-buffer-name "*CLAUDE-CODE*"
  "Buffer name where Claude prompts appear."
  :type 'string
  :group 'emacs-claude)

(defvar emacs-claude-buffer nil
  "Buffer object where Claude prompts appear.")

(defvar emacs-claude-code-timer nil
  "Timer object for polling Claude prompts.")

(defcustom emacs-claude-prompt-y/n "❯ 1. Yes"
  "Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-prompt-y/y/n
  " 2. Yes, and don't ask again"
  "y/Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-prompt-for-waiting
  "Based on the documents under ./docs, find room for improvement and refine the codebase. \n"
  "Prompt to send to waiting situation"
  :type 'string
  :group 'emacs-claude)

;; (defcustom emacs-claude-prompt-waiting
;;   "│ >                                                                                                   │"
;;   "Pattern that matches the waiting prompt shown in Claude interface."
;;   :type 'string
;;   :group 'emacs-claude)

(defcustom emacs-claude-prompt-waiting
  "╭─────────────────────────────────────────────────────────────────────────────────────────────────────╮
│ >                                                                                                   │
╰─────────────────────────────────────────────────────────────────────────────────────────────────────╯"
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-prompt-initial-waiting
  "│ > Try "
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-code-interval-sec 1
  "Interval in seconds between checks for Claude prompts."
  :type 'number
  :group 'emacs-claude)

;; Additional prompt patterns

(defcustom emacs-claude-prompt-error
  "Error: "
  "Pattern that matches error messages from Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-prompt-loading
  "Loading..."
  "Pattern that matches loading indicator in Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-prompt-thinking
  "Thinking..."
  "Pattern that matches thinking indicator in Claude interface."
  :type 'string
  :group 'emacs-claude)


(provide 'emacs-claude-code-variables)

(when
    (not load-file-name)
  (message "emacs-claude-code-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))