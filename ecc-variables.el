;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:34>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defcustom ecc-buffer-name "*CLAUDE-CODE*"
  "Buffer name where Claude prompts appear."
  :type 'string
  :group 'emacs-claude)

(defvar ecc-buffer nil
  "Buffer object where Claude prompts appear (deprecated, use ecc-active-buffer).")

(defvar ecc-buffers nil
  "List of buffer objects where Claude prompts appear.")

(defvar ecc-active-buffer nil
  "Currently active Claude buffer that receives commands.")

(defvar ecc-timer nil
  "Timer object for polling Claude prompts.")

(defcustom ecc-prompt-y/n "❯ 1. Yes"
  "Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-y/y/n
  " 2. Yes, and don't ask again"
  "y/Y/n prompt pattern to match when Claude asks for confirmation."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-for-waiting
  "Based on the documents under ./docs, find room for improvement and refine the codebase. \n"
  "Prompt to send to waiting situation"
  :type 'string
  :group 'emacs-claude)

;; (defcustom ecc-prompt-waiting
;;   "│ >                                                                                                   │"
;;   "Pattern that matches the waiting prompt shown in Claude interface."
;;   :type 'string
;;   :group 'emacs-claude)

(defcustom ecc-prompt-waiting
  "╭─────────────────────────────────────────────────────────────────────────────────────────────────────╮
│ >                                                                                                   │
╰─────────────────────────────────────────────────────────────────────────────────────────────────────╯"
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-initial-waiting
  "│ > Try "
  "Pattern that matches the waiting prompt shown in Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-interval-sec 1
  "Interval in seconds between checks for Claude prompts."
  :type 'number
  :group 'emacs-claude)

;; Additional prompt patterns

(defcustom ecc-prompt-error
  "Error: "
  "Pattern that matches error messages from Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-loading
  "Loading..."
  "Pattern that matches loading indicator in Claude interface."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-prompt-thinking
  "Thinking..."
  "Pattern that matches thinking indicator in Claude interface."
  :type 'string
  :group 'emacs-claude)

(defvar ecc-running nil
  "Non-nil when Claude is running and processing a request.")

(defcustom ecc-running-mode-line " [Claude:Running]"
  "String to display in mode line when Claude is running."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-waiting-mode-line " [Claude:Waiting]"
  "String to display in mode line when Claude is waiting for input."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-idle-mode-line " [Claude:Idle]"
  "String to display in mode line when Claude is idle."
  :type 'string
  :group 'emacs-claude)


(provide 'ecc-variables)

(when
    (not load-file-name)
  (message "ecc-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))