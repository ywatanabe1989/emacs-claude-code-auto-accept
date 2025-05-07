;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:25>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-buffer-registry.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ecc-variables)

(defvar ecc-buffer-timestamps (make-hash-table :test 'equal)
  "Hash table mapping Claude buffers to their creation timestamps.")

(defun ecc-buffer-ensure-active-exists ()
  "Ensure Claude buffer exists and is properly set up."
  (unless (and (buffer-live-p ecc-active-buffer)
               (with-current-buffer ecc-active-buffer
                 (derived-mode-p 'vterm-mode)))
    (setq ecc-active-buffer
          (get-buffer-create ecc-buffer-name))
    (with-current-buffer ecc-active-buffer
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode))
      (rename-buffer ecc-buffer-name)
      (sit-for 0.5)
      (vterm-send-string "claude")
      (vterm-send-return)))
  ecc-active-buffer)

(defun ecc-buffer-rename-buffer (enable)
  "Rename buffer according to auto-accept status.
If ENABLE is non-nil, rename to special Claude name.
Otherwise, reset to a default vterm name."
  (when (buffer-live-p ecc-active-buffer)
    (with-current-buffer ecc-active-buffer
      (let ((current-name (buffer-name))
            (default-name
             (concat "vterm<" (number-to-string (emacs-pid)) ">")))
        (if enable
            (unless (string-match-p (regexp-quote ecc-buffer-name) current-name)
              (rename-buffer ecc-buffer-name t))
          (when (string-match-p (regexp-quote ecc-buffer-name) current-name)
            (rename-buffer default-name t)))))))

;; (defun --ecc-buffer-register-buffer (buffer)
;;   "Register BUFFER as a Claude buffer with timestamp."
;;   (unless (member buffer ecc-buffers)
;;     (push buffer ecc-buffers)
;;     (puthash buffer (current-time) ecc-buffer-timestamps))
;;   (setq ecc-active-buffer buffer)
;;   (ecc-update-mode-line-all-buffers))
(defun --ecc-buffer-register-buffer (buf)
  "Register BUF as a Claude buffer."
  (unless (member buf ecc-buffers)
    (push buf ecc-buffers))
  (setq ecc-active-buffer buf)
  (ecc-update-mode-line-all-buffers)
  buf)


(defun ecc-buffer-list-buffers ()
  "List all registered Claude buffers."
  (interactive)
  (let ((buffer-names
         (mapcar (lambda (buf)
                   (format "%s%s"
                           (if (eq buf ecc-active-buffer)
                               "* "
                             "  ")
                           (buffer-name buf)))
                 ecc-buffers)))
    (message "Claude buffers:\n%s"
             (mapconcat 'identity buffer-names "\n"))))

(defun ecc-buffer-unregister-buffer (buffer-or-name)
  "Unregister a buffer from Claude buffers."
  (interactive
   (list (completing-read "Unregister Claude buffer: "
                          (mapcar 'buffer-name ecc-buffers))))
  (let ((buffer (get-buffer buffer-or-name)))
    (when (member buffer ecc-buffers)
      (setq ecc-buffers (delq buffer ecc-buffers))
      (when (eq buffer ecc-active-buffer)
        (setq ecc-active-buffer
              (car ecc-buffers)))
      (ecc-update-mode-line-all-buffers)
      (message "Unregistered %s from Claude buffers"
               (buffer-name buffer)))))

(defun --ecc-buffer-name-change-hook ()
  "Hook to prevent Claude from changing the buffer name."
  (let ((current-name (buffer-name))
        (original-name
         (buffer-local-value 'ecc-original-name
                             (current-buffer))))
    (when (and original-name
               (not (string= current-name original-name)))
      (rename-buffer original-name t))))

;;;###autoload
(defun ecc-buffer-registry-cleanup-buffers ()
  "Remove dead buffers from Claude buffer list."
  (setq ecc-buffers
        (cl-remove-if-not 'buffer-live-p ecc-buffers))
  (unless (and (buffer-live-p ecc-active-buffer)
               (member ecc-active-buffer ecc-buffers))
    (setq ecc-active-buffer (car ecc-buffers))))


(provide 'ecc-buffer-registry)

(when
    (not load-file-name)
  (message "ecc-buffer-registry.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
