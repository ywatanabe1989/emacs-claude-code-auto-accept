;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 11:03:07>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-run.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-variables)
(declare-function vterm-mode "ext:vterm")
(declare-function vterm-send-string "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-clear "ext:vterm")

(defcustom ecc-prompt-template
  "Please help me with the following:\n\n%s"
  "Template for formatting prompts to Claude."
  :type 'string
  :group 'emacs-claude)

(defcustom ecc-default-instructions
  "Analyze this code and suggest improvements."
  "Default instructions to use when no specific prompt is provided."
  :type 'string
  :group 'emacs-claude)


;;;###autoload
(defun ecc-run (prompt &optional content)
  "Send PROMPT and optional CONTENT to Claude buffer."
  (interactive "sPrompt for Claude: ")
  (with-current-buffer (ecc-buffer-ensure-active-exists)
    (vterm-clear)
    (sit-for 0.5)
    (let ((formatted-prompt
           (format ecc-prompt-template
                   (if content
                       (concat prompt "\n\n" content)
                     prompt))))
      (vterm-send-string formatted-prompt)
      (vterm-send-return)
      (message "Prompt sent to Claude."))))

;;;###autoload
(defun ecc-run-on-region (start end &optional prompt)
  "Send region from START to END with optional PROMPT to Claude buffer."
  (interactive "r\nsPrompt for Claude (leave empty for default): ")
  (let* ((region-content (buffer-substring-no-properties start end))
         (prompt-text (if (string-empty-p prompt)
                          ecc-default-instructions
                        prompt)))
    (ecc-run prompt-text region-content)))

;;;###autoload
(defun ecc-run-on-buffer (&optional prompt)
  "Send current buffer with optional PROMPT to Claude buffer."
  (interactive "sPrompt for Claude (leave empty for default): ")
  (let* ((buffer-content (buffer-substring-no-properties
                          (point-min) (point-max)))
         (prompt-text (if (string-empty-p prompt)
                          ecc-default-instructions
                        prompt))
         (buffer-name (buffer-name)))
    (ecc-run
     (format "%s\n\nFile: %s" prompt-text buffer-name)
     buffer-content)))

;;;###autoload
(defun ecc-run-quick ()
  "Quickly ask Claude a question from minibuffer."
  (interactive)
  (let ((question (read-string "Ask Claude: ")))
    (ecc-run question)))


(provide 'ecc-run)

(when
    (not load-file-name)
  (message "ecc-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
