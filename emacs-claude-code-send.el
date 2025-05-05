;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 03:37:20>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'emacs-claude-code-variables)
(require 'emacs-claude-code-detect-prompt)

;; External function declarations
(declare-function vterm-clear "ext:vterm")
(declare-function vterm-send-string "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-copy-mode "ext:vterm")

(defun emacs-claude-code-send ()
  "Automatically respond to Claude prompts in vterm."
  (interactive)
  (when (buffer-live-p emacs-claude-buffer)
    (with-current-buffer emacs-claude-buffer
      (vterm-clear)
      (cond
       ((--emacs-claude-detect-prompt-y/y/n)
        (--emacs-claude-auto-send-yy))
       ((--emacs-claude-detect-prompt-y/n)
        (--emacs-claude-auto-send-y))
       ((--emacs-claude-detect-prompt-waiting)
        (--emacs-claude-auto-send-continue))
       ((--emacs-claude-detect-prompt-initial-waiting)
        (--emacs-claude-auto-send-continue))))))

;; Sub-functions
;; ------------------------------

(defun --emacs-claude-auto-send-y ()
  "Automatically respond with '1' to Claude prompts in vterm."
  (interactive)
  (with-current-buffer emacs-claude-buffer
    (when (--emacs-claude-detect-prompt-y/n)
      (sit-for 1.0)
      (vterm-send-string "1\n")
      (vterm-send-return)
      (sit-for 1.0)
      (vterm-copy-mode -1)
      (message "[emacs-claude-code-send] Automatic Response: 1"))))

(defun --emacs-claude-auto-send-yy ()
  "Automatically respond with '2' to Claude prompts in vterm."
  (interactive)
  (with-current-buffer emacs-claude-buffer
    (when (--emacs-claude-detect-prompt-y/y/n)
      (sit-for 1.0)
      (vterm-send-string "2\n")
      (vterm-send-return)
      (vterm-copy-mode -1)
      (sit-for 1.0)
      (message "[emacs-claude-code-send] Automatic Response: 2"))))

(defun --emacs-claude-auto-send-continue ()
  "Automatically respond with 'continue' to Claude waiting prompts in vterm."
  (interactive)
  (with-current-buffer emacs-claude-buffer
    (when (or (--emacs-claude-detect-prompt-waiting)
              (--emacs-claude-detect-prompt-initial-waiting))
      (sit-for 1.0)
      (vterm-send-string
       emacs-claude-prompt-for-waiting)
      (vterm-send-return)
      (vterm-copy-mode -1)
      (sit-for 1.0)
      (message
       "[emacs-claude-code-send] Automatic Response: continue"))))


(provide 'emacs-claude-code-send)

(when
    (not load-file-name)
  (message "emacs-claude-code-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))