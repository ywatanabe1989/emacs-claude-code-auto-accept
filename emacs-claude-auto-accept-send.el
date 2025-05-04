;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: root
;;; Timestamp: <2025-05-05 00:23:10>
;;; File: /root/.emacs.d/lisp/emacs-claude-code-auto-accept/emacs-claude-auto-accept-send.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun emacs-claude-auto-accept-send ()
  "Automatically respond to Claude prompts in vterm."
  (interactive)
  (when (get-buffer emacs-claude-buffer-name)
    (with-current-buffer emacs-claude-buffer-name
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
  (with-current-buffer emacs-claude-buffer-name
    (when (--emacs-claude-detect-prompt-y/n)
      (sit-for 0.3)
      (vterm-send-string "1\n")
      (vterm-send-return)
      (sit-for 0.3)
      (vterm-copy-mode -1)
      (message "[emacs-claude-auto-accept-send] Automatic Response: 1"))))

(defun --emacs-claude-auto-send-yy ()
  "Automatically respond with '2' to Claude prompts in vterm."
  (interactive)
  (with-current-buffer emacs-claude-buffer-name
    (when (--emacs-claude-detect-prompt-y/y/n)
      (sit-for 0.3)
      (vterm-send-string "2\n")
      (vterm-send-return)
      (vterm-copy-mode -1)
      (sit-for 0.3)
      (message "[emacs-claude-auto-accept-send] Automatic Response: 2"))))

(defun --emacs-claude-auto-send-continue ()
  "Automatically respond with 'continue' to Claude waiting prompts in vterm."
  (interactive)
  (with-current-buffer emacs-claude-buffer-name
    (when (or (--emacs-claude-detect-prompt-waiting)
              (--emacs-claude-detect-prompt-initial-waiting))
      (sit-for 0.3)
      (vterm-send-string "continue\n")
      (vterm-send-return)
      (vterm-copy-mode -1)
      (sit-for 0.3)
      (message
       "[emacs-claude-auto-accept-send] Automatic Response: continue"))))

(provide 'emacs-claude-auto-accept-send)

(when
    (not load-file-name)
  (message "emacs-claude-auto-accept-send.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))