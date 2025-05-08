;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 03:37:06>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code_v01/ecc-buffer/playground.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(let ((new-buffer (ecc-buffer-create-registered-buffer t)))
  (ecc-buffer-set-current-buffer new-buffer)
  (ecc-buffer-get-current-buffer)
  (ecc-buffer-get-buffer-state new-buffer)
  (ecc-buffer-set-buffer-state new-buffer 'waiting)
  (ecc-buffer-get-buffer-state new-buffer)
  ;; (ecc-buffer-set-current-buffer-state new-buffer)
  )

;; ;; Create a new Claude buffer
;; (ecc-buffer-create-registered-buffer)
;; (ecc-buffer-list-registered-buffers)

;; ;; Check the current state of Claude
;; (display-buffer (get-buffer "*CLAUDE-CODE-01*"))
;; (ecc-state-get (get-buffer "*CLAUDE-CODE-09*"))

;; ;; Send a command when Claude is waiting
;; (when (eq (ecc-state-get) :waiting)
;;   (--ecc-send-string "Tell me about Emacs Lisp"))

;; ;; Navigate between multiple Claude sessions
;; (ecc-buffer-next)
;; (ecc-buffer-prev)


(provide 'playground)

(when
    (not load-file-name)
  (message "playground.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))