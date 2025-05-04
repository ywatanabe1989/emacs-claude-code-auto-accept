;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: root
;;; Timestamp: <2025-05-04 22:59:57>
;;; File: /root/.emacs.d/lisp/claude/emacs-claude-auto-accept-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun emacs-claude-auto-accept-send ()
  "Automatically respond to Claude prompts in vterm."
  (interactive)
  (when (get-buffer emacs-claude-buffer-name)
    (cond
     ((--emacs-claude-detect-prompt-y/y/n)
      (with-current-buffer emacs-claude-buffer-name
        (--emacs-claude-auto-send-yy)))
     ((--emacs-claude-detect-prompt-y/n)
      (with-current-buffer emacs-claude-buffer-name
        (--emacs-claude-auto-send-y)))
     ((--emacs-claude-detect-prompt-waiting)
      (with-current-buffer emacs-claude-buffer-name
        (--emacs-claude-auto-send-continue))))))

;; Sub-functions
;; ------------------------------

(defun --emacs-claude-auto-send-y ()
  "Automatically respond with '1' to Claude prompts in vterm."
  (interactive)
  (when (get-buffer emacs-claude-buffer-name)
    (when (--emacs-claude-detect-prompt-y/n)
      (with-current-buffer emacs-claude-buffer-name
        (vterm-send-string "1\n")
        (vterm-send-return)))))

(defun --emacs-claude-auto-send-yy ()
  "Automatically respond with '2' to Claude prompts in vterm."
  (interactive)
  (when (get-buffer emacs-claude-buffer-name)
    (when (--emacs-claude-detect-prompt-y/y/n)
      (with-current-buffer emacs-claude-buffer-name
        (vterm-send-string "2\n")
        (vterm-send-return)))))

(defun --emacs-claude-auto-send-continue ()
  "Automatically respond with 'continue' to Claude waiting prompts in vterm."
  (interactive)
  (when (get-buffer emacs-claude-buffer-name)
    (when (--emacs-claude-detect-prompt-waiting)
      (with-current-buffer emacs-claude-buffer-name
        (vterm-send-string "continue\n")
        (vterm-send-return)))))

(provide 'emacs-claude-auto-accept-send)

(when
    (not load-file-name)
  (message "emacs-claude-auto-accept-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))