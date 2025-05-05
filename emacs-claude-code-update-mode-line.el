;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:29:55>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code-update-mode-line.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'emacs-claude-code-variables)


(defvar emacs-claude-code-mode-line-indicator " [Claude-Auto]"
  "Mode line indicator for Claude auto-accept mode.")

(defvar-local emacs-claude-code-buffer-name-overlay nil
  "Overlay for highlighting the buffer name in mode line.")

(defun emacs-claude-code-update-mode-line (enable)
  "Update mode line with indicator when auto-accept is enabled."
  (if enable
      (progn
        ;; Add mode line indicator
        (unless (member emacs-claude-code-mode-line-indicator
                        global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '(""))
                        (list
                         emacs-claude-code-mode-line-indicator))))
        ;; Highlight buffer name instead of whole mode line
        (when (buffer-live-p emacs-claude-buffer)
          (with-current-buffer emacs-claude-buffer
            (when emacs-claude-code-buffer-name-overlay
              (delete-overlay
               emacs-claude-code-buffer-name-overlay))
            (setq emacs-claude-code-buffer-name-overlay
                  (make-overlay (point-min) (point-min)))
            (overlay-put emacs-claude-code-buffer-name-overlay
                         'before-string
                         (propertize (buffer-name)
                                     'face
                                     '(:background "orange"
                                                   :foreground
                                                   "black"))))))
    ;; Remove indicators
    (setq global-mode-string
          (remove emacs-claude-code-mode-line-indicator
                  global-mode-string))
    (when (buffer-live-p emacs-claude-buffer)
      (with-current-buffer emacs-claude-buffer
        (when emacs-claude-code-buffer-name-overlay
          (delete-overlay emacs-claude-code-buffer-name-overlay)
          (setq emacs-claude-code-buffer-name-overlay nil)))))
  (force-mode-line-update t))


(provide 'emacs-claude-code-update-mode-line)

(when
    (not load-file-name)
  (message "emacs-claude-code-update-mode-line.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))