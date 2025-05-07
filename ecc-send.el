;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:28>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-variables)
(require 'ecc-state)

;; External function declarations
(declare-function vterm-clear "ext:vterm")
(declare-function vterm-send-string "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-copy-mode "ext:vterm")

(defun ecc-send-paste-to-active-buffer ()
  "Paste the current kill ring contents to Claude buffer."
  (interactive)
  (when (buffer-live-p ecc-active-buffer)
    (with-current-buffer ecc-active-buffer
      (let ((content (current-kill 0)))
        (vterm-send-string content)
        (message "Content pasted to Claude buffer.")))))

(defun ecc-send-accept ()
  "Automatically respond to Claude prompts in active vterm."
  (interactive)
  (when (buffer-live-p ecc-active-buffer)
    (with-current-buffer ecc-active-buffer
      (vterm-clear)
      (cond
       ((--ecc-state-y/y/n-p)
        (--ecc-auto-send-2-y/y/n))
       ((--ecc-state-y/n-p)
        (--ecc-auto-send-1-y/n))
       ((--ecc-state-waiting-p)
        (--ecc-auto-send-continue))
       ((--ecc-state-initial-waiting-p)
        (--ecc-auto-send-continue))))))

(defun ecc-send-string (string &optional confirm delay)
  "Send STRING to the active Claude buffer.
If CONFIRM is non-nil, send return after the string.
DELAY is the time to wait after sending (defaults to 0.5 seconds)."
  (when (buffer-live-p ecc-active-buffer)
    (with-current-buffer ecc-active-buffer
      (when delay (sit-for delay))
      (vterm-send-string string)
      (when confirm (vterm-send-return))
      (vterm-copy-mode -1)
      (when delay (sit-for delay))
      (message "[ecc-send] Sent: %s" string))))

(defun --ecc-auto-send-by-state (response state-or-predicate)
  "Send RESPONSE to a specific Claude prompt state.
STATE-OR-PREDICATE can be a keyword (:y/n, :y/y/n, etc.) or a predicate function."
  (with-current-buffer ecc-active-buffer
    (let ((should-send
           (if (functionp state-or-predicate)
               (funcall state-or-predicate)
             (eq (ecc-state-get) state-or-predicate))))
      (when should-send
        (ecc-send-string response t 1.0)
        (message "[ecc-send] Automatic Response: %s" response)))))

(defun --ecc-auto-send-1-y/n ()
  "Automatically respond with '1' to Claude prompts in vterm."
  (interactive)
  (--ecc-auto-send-by-state "1" :y/n))

(defun --ecc-auto-send-2-y/y/n ()
  "Automatically respond with '2' to Claude prompts in vterm."
  (interactive)
  (--ecc-auto-send-by-state "2" :y/y/n))

(defun --ecc-auto-send-3 ()
  "Automatically respond with '3' to Claude prompts in vterm."
  (interactive)
  (--ecc-auto-send-by-state "3" :y/y/n))

(defun --ecc-auto-send-custom (CUSTOME-TEXT)
  "Automatically respond with CUSTOM-TEXT to Claude prompts in vterm."
  (interactive)
  (--ecc-auto-send-by-state CUSTOM-TEXT :y/y/n))

(defun ecc-send-template ()
  "Interactively input and send a custom template response for y/y/n situations."
  (interactive)
  (let ((template-text (read-string "Enter custom response: ")))
    (--ecc-auto-send-template template-text)))

(defun --ecc-auto-send-continue ()
  "Automatically respond with continue to Claude waiting prompts."
  (interactive)
  (--ecc-auto-send-by-state
   "continue"
   (lambda ()
     (or (--ecc-state-waiting-p)
         (--ecc-state-initial-waiting-p)))))

(defun --ecc-auto-send-skip ()
  "Automatically respond with skip to Claude waiting prompts."
  (interactive)
  (--ecc-auto-send-by-state
   "skip"
   (lambda ()
     (or (--ecc-state-waiting-p)
         (--ecc-state-initial-waiting-p)))))


;;;###autoload
(defun ecc-send-interrupt ()
  "Interrupt the currently running Claude process.
This sends Ctrl-C to the terminal to stop any ongoing operation."
  (interactive)
  (when (buffer-live-p ecc-active-buffer)
    (with-current-buffer ecc-active-buffer
      ;; Send Ctrl-C by properly using vterm-send-key with control modifier
      (vterm-send-key "c" nil t)
      (sit-for 0.3)
      (setq ecc-running nil)
      (ecc-update-mode-line-all-buffers)
      (message "Claude process interrupted."))))

;;;###autoload
(defun ecc-send-init ()
  "Clear the current Claude Code conversation and restart it.
This exits the terminal and starts a new Claude session."
  (interactive)
  (when (buffer-live-p ecc-active-buffer)
    (with-current-buffer ecc-active-buffer
      ;; Interrupt any running process first
      (ecc-send-interrupt)
      (sit-for 0.3)
      
      ;; Clear screen
      (vterm-clear)
      (sit-for 0.3)
      
      ;; Exit current process if any
      (ecc-send-string "exit" t 0.7)
      
      ;; Clear screen again to ensure a clean slate
      (vterm-clear)
      (sit-for 0.3)
      
      ;; Start a new Claude session
      (ecc-send-string "claude" t 0.5)
      
      (message "Claude conversation restarted."))))


;;;###autoload
(defun ecc-send-clear-history ()
  "Clear the conversation history in Claude Code.
This sends the '/clear-history' command to Claude."
  (interactive)
  (ecc-send-string "/clear-history" t 0.5)
  (message "Claude conversation history cleared."))

;;;###autoload
(defun ecc-send-compact-history ()
  "Compact the conversation history in Claude Code to save tokens.
This sends the '/compact-history' command to Claude."
  (interactive)
  (ecc-send-string "/compact-history" t 0.5)
  (message "Claude conversation history compacted."))

;;;###autoload
(defun ecc-send-edit-memory ()
  "Edit Claude's memory/previous messages in the conversation.
This sends the '/edit-memory' command to Claude."
  (interactive)
  (ecc-send-string "/edit-memory" t 0.5)
  (message "Claude memory edit mode enabled."))

(provide 'ecc-send)

(when
    (not load-file-name)
  (message "ecc-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
