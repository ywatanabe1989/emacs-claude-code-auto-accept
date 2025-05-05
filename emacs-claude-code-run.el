;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:29:54>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code-run.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'emacs-claude-code-variables)
(declare-function vterm-mode "ext:vterm")
(declare-function vterm-send-string "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-clear "ext:vterm")

(defcustom emacs-claude-prompt-template
  "Please help me with the following:\n\n%s"
  "Template for formatting prompts to Claude."
  :type 'string
  :group 'emacs-claude)

(defcustom emacs-claude-default-instructions
  "Analyze this code and suggest improvements."
  "Default instructions to use when no specific prompt is provided."
  :type 'string
  :group 'emacs-claude)

(defun emacs-claude-ensure-buffer ()
  "Ensure Claude buffer exists and is properly set up."
  (unless (and (buffer-live-p emacs-claude-buffer)
               (with-current-buffer emacs-claude-buffer
                 (derived-mode-p 'vterm-mode)))
    (setq emacs-claude-buffer
          (get-buffer-create emacs-claude-buffer-name))
    (with-current-buffer emacs-claude-buffer
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode))
      (rename-buffer emacs-claude-buffer-name)
      (sit-for 0.5)
      (vterm-send-string "claude")
      (vterm-send-return)))
  emacs-claude-buffer)

(defun emacs-claude-run (prompt &optional content)
  "Send PROMPT and optional CONTENT to Claude buffer."
  (interactive "sPrompt for Claude: ")
  (with-current-buffer (emacs-claude-ensure-buffer)
    (vterm-clear)
    (sit-for 0.5)
    (let ((formatted-prompt
           (format emacs-claude-prompt-template
                   (if content
                       (concat prompt "\n\n" content)
                     prompt))))
      (vterm-send-string formatted-prompt)
      (vterm-send-return)
      (message "Prompt sent to Claude."))))

(defun emacs-claude-run-on-region (start end &optional prompt)
  "Send region from START to END with optional PROMPT to Claude buffer."
  (interactive "r\nsPrompt for Claude (leave empty for default): ")
  (let* ((region-content (buffer-substring-no-properties start end))
         (prompt-text (if (string-empty-p prompt)
                          emacs-claude-default-instructions
                        prompt)))
    (emacs-claude-run prompt-text region-content)))

(defun emacs-claude-run-on-buffer (&optional prompt)
  "Send current buffer with optional PROMPT to Claude buffer."
  (interactive "sPrompt for Claude (leave empty for default): ")
  (let* ((buffer-content (buffer-substring-no-properties
                          (point-min) (point-max)))
         (prompt-text (if (string-empty-p prompt)
                          emacs-claude-default-instructions
                        prompt))
         (buffer-name (buffer-name)))
    (emacs-claude-run
     (format "%s\n\nFile: %s" prompt-text buffer-name)
     buffer-content)))

(defun emacs-claude-run-quick ()
  "Quickly ask Claude a question from minibuffer."
  (interactive)
  (let ((question (read-string "Ask Claude: ")))
    (emacs-claude-run question)))


(provide 'emacs-claude-code-run)

(when
    (not load-file-name)
  (message "emacs-claude-code-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))