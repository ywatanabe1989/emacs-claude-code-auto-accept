;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:13:20>
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

(defun --ecc-auto-send-by-state (response state-or-predicate)
  "Send RESPONSE to a specific Claude prompt state.
STATE-OR-PREDICATE can be a keyword (:y/n, :y/y/n, etc.) or a predicate function."
  (with-current-buffer ecc-active-buffer
    (let ((should-send
           (if (functionp state-or-predicate)
               (funcall state-or-predicate)
             (eq (ecc-state-get) state-or-predicate))))
      (when should-send
        (sit-for 1.0)
        (vterm-send-string response)
        (vterm-send-return)
        (vterm-copy-mode -1)
        (sit-for 1.0)
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

;; (defun --ecc-auto-send-continue ()
;;   "Automatically respond with continue to Claude waiting prompts."
;;   (interactive)
;;   (--ecc-auto-send-by-state
;;    ecc-prompt-for-waiting
;;    (lambda ()
;;      (or (--ecc-state-waiting-p)
;;          (--ecc-state-initial-waiting-p)))))


(provide 'ecc-send)

(when
    (not load-file-name)
  (message "ecc-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))