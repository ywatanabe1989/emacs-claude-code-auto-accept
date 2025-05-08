;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 03:18:06>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/ecc-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-variables)

(defvar ecc-buffer-current-active-buffer nil
  "The current active Claude buffer.")

;;;###autoload
(defun ecc-buffer-get-or-create-active-buffer ()
  "Get or create Claude active buffer."
  (unless (and (buffer-live-p ecc-buffer-current-active-buffer)
               (with-current-buffer ecc-buffer-current-active-buffer
                 (derived-mode-p 'vterm-mode)))
    (setq ecc-buffer-current-active-buffer
          (get-buffer-create ecc-buffer-name))
    (with-current-buffer ecc-buffer-current-active-buffer
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode))
      (rename-buffer ecc-buffer-name)
      (sit-for 0.5)
      (vterm-send-string "claude")
      (vterm-send-return)))
  (display-buffer ecc-buffer-current-active-buffer)
  ecc-buffer-current-active-buffer)

(defun --ecc-state-is-claude-active-p (&optional buffer)
  "Check if BUFFER is an active Claude buffer.
If BUFFER is not provided, check current buffer."
  (with-current-buffer (or buffer ecc-buffer-current-active-buffer)
    (derived-mode-p 'vterm-mode)))

(provide 'ecc-buffer)

(when (not load-file-name)
  (message "ecc-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))