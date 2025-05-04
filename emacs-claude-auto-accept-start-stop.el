;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: root
;;; Timestamp: <2025-05-04 22:51:38>
;;; File: /root/.emacs.d/lisp/claude/emacs-claude-auto-accept-start-stop.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun emacs-claude-auto-accept-start ()
  "Start auto-accepting Claude prompts using a timer."
  (interactive)
  (unless (get-buffer emacs-claude-buffer-name)
    (rename-buffer emacs-claude-buffer-name t))
  (add-hook 'vterm-update-functions 'emacs-claude-auto-accept-send)
  (message "Claude auto-accept enabled for %s"
           emacs-claude-buffer-name))

(defun emacs-claude-auto-accept-stop ()
  "Stop auto-accepting Claude prompts."
  (interactive)
  (remove-hook 'vterm-update-functions 'emacs-claude-auto-accept-send)
  (message "Claude auto-accept disabled"))

(provide 'emacs-claude-auto-accept-start-stop)

(when
    (not load-file-name)
  (message "emacs-claude-auto-accept-start-stop.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))