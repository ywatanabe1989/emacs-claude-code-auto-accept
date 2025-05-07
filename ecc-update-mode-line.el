;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 11:03:08>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-update-mode-line.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-variables)

(defvar ecc-mode-line-indicator " [Claude-Auto]"
  "Mode line indicator for Claude auto-accept mode.")

(defvar-local ecc-buffer-name-overlay nil
  "Overlay for highlighting the buffer name in mode line.")

(defun ecc-update-mode-line (enable)
  "Update mode line with indicator when auto-accept is enabled."
  (if enable
      (progn
        ;; Add mode line indicator
        (unless (member ecc-mode-line-indicator
                        global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '(""))
                        (list
                         ecc-mode-line-indicator))))
        ;; Highlight buffer name instead of whole mode line
        (when (buffer-live-p ecc-active-buffer)
          (with-current-buffer ecc-active-buffer
            (when ecc-buffer-name-overlay
              (delete-overlay
               ecc-buffer-name-overlay))
            (setq ecc-buffer-name-overlay
                  (make-overlay (point-min) (point-min)))
            (overlay-put ecc-buffer-name-overlay
                         'before-string
                         (propertize (buffer-name)
                                     'face
                                     '(:background "orange"
                                                   :foreground
                                                   "black"))))))
    ;; Remove indicators
    (setq global-mode-string
          (remove ecc-mode-line-indicator
                  global-mode-string))
    (when (buffer-live-p ecc-active-buffer)
      (with-current-buffer ecc-active-buffer
        (when ecc-buffer-name-overlay
          (delete-overlay ecc-buffer-name-overlay)
          (setq ecc-buffer-name-overlay nil)))))
  (force-mode-line-update t))

(defun ecc-update-mode-line-all-buffers ()
  "Update mode line for all Claude buffers."
  (dolist (buf ecc-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((is-active (eq buf ecc-active-buffer)))
          (when ecc-buffer-name-overlay
            (delete-overlay ecc-buffer-name-overlay))
          (setq ecc-buffer-name-overlay
                (make-overlay (point-min) (point-min)))
          (overlay-put ecc-buffer-name-overlay
                       'before-string
                       (propertize (buffer-name)
                                   'face
                                   `(:background
                                     ,(if is-active "orange" "gray")
                                     :foreground "black"))))))))


(provide 'ecc-update-mode-line)

(when
    (not load-file-name)
  (message "ecc-update-mode-line.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))