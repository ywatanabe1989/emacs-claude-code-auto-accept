;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 11:24:21>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-buffer-navigation.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defun ecc-buffer-new (&optional new-buffer-name)
  "Create a new Claude buffer and register it."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (original-point (point))
         (buffer-name
          (or new-buffer-name
              (format "*CLAUDE-CODE-%02d*"
                      (length ecc-buffers))))
         (new-buffer (get-buffer-create buffer-name)))
    (switch-to-buffer-other-window new-buffer)
    (with-current-buffer new-buffer
      (vterm-mode)
      (set (make-local-variable 'ecc-original-name)
           buffer-name)
      (add-hook 'after-change-functions
                (lambda (&rest _)
                  (--ecc-buffer-name-change-hook))
                nil t)
      (sit-for 0.5)
      (vterm-send-string "claude")
      (vterm-send-return))
    (--ecc-buffer-register-buffer new-buffer)))

(defun ecc-buffer-next ()
  "Switch to next Claude buffer."
  (interactive)
  (when ecc-buffers
    (let*
        ((buffers
          (cl-remove-if-not #'buffer-live-p ecc-buffers))
         (sorted-buffers (sort (copy-sequence buffers)
                               (lambda (b1 b2)
                                 (time-less-p
                                  (gethash b1
                                           ecc-buffer-timestamps)
                                  (gethash b2
                                           ecc-buffer-timestamps)))))
         (pos
          (cl-position ecc-active-buffer sorted-buffers))
         (next-pos
          (mod (if (null pos) 0 (1+ pos)) (length sorted-buffers)))
         (next-buffer (nth next-pos sorted-buffers)))
      (when next-buffer
        (setq ecc-active-buffer next-buffer)
        (ecc-update-mode-line-all-buffers)
        (display-buffer next-buffer)))))

(defun ecc-buffer-prev ()
  "Switch to previous Claude buffer."
  (interactive)
  (when ecc-buffers
    (let*
        ((buffers
          (cl-remove-if-not #'buffer-live-p ecc-buffers))
         (sorted-buffers (sort (copy-sequence buffers)
                               (lambda (b1 b2)
                                 (time-less-p
                                  (gethash b1
                                           ecc-buffer-timestamps)
                                  (gethash b2
                                           ecc-buffer-timestamps)))))
         (pos
          (cl-position ecc-active-buffer sorted-buffers))
         (prev-pos
          (mod (if (null pos) 0 (1- pos)) (length sorted-buffers)))
         (prev-buffer (nth prev-pos sorted-buffers)))
      (when prev-buffer
        (setq ecc-active-buffer prev-buffer)
        (ecc-update-mode-line-all-buffers)
        (display-buffer prev-buffer)))))


(provide 'ecc-buffer-navigation)

(when
    (not load-file-name)
  (message "ecc-buffer-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))