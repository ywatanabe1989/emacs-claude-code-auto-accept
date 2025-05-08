;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 14:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/ecc-buffer-list.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ecc-variables)
(require 'ecc-buffer)

;;;###autoload
(defun ecc-buffer-list-show ()
  "Show a list of all active Claude buffers including dedicated buffers.
This function wraps the functionality provided by ecc-buffer-list-buffers
but ensures any dedicated buffer management is also displayed."
  (interactive)
  ;; First, verify we have the functions we need
  (if (fboundp 'ecc-buffer-list-interactive)
      ;; Use the interactive buffer list mode
      (call-interactively 'ecc-buffer-list-interactive)
    ;; Use the basic buffer list function
    (ecc-buffer-list-buffers))
  
  ;; Add a message about the number of buffers for clarity
  (let ((buffer-count (ecc-buffer-count)))
    (message "Showing %d registered Claude %s"
             buffer-count
             (if (= buffer-count 1) "buffer" "buffers"))))

;;;###autoload
(defun demo-list-buffers ()
  "Show a list of all active Claude buffers."
  (interactive)
  (ecc-buffer-list-show))

(provide 'ecc-buffer-list)

(when
    (not load-file-name)
  (message "ecc-buffer-list.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))