;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: root
;;; Timestamp: <2025-05-04 22:56:57>
;;; File: /root/.emacs.d/lisp/claude/emacs-claude-auto-accept-detect-prompt.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; Detectors
;; ------------------------------

(defun --emacs-claude-detect-prompt-waiting ()
  "Detect waiting prompt in Claude buffer."
  (interactive)
  (let
      ((result
        (--emacs-claude-detect-prompt emacs-claude-prompt-waiting)))
    (when result
      t)))

(defun --emacs-claude-detect-prompt-y/n ()
  "Detect y/n prompt in Claude buffer."
  (interactive)
  (let
      ((result (and
                (--emacs-claude-detect-prompt emacs-claude-prompt-y/n)
                (not (--emacs-claude-detect-prompt-y/y/n)))))
    (when result
      t)))

(defun --emacs-claude-detect-prompt-y/y/n ()
  "Detect y/n prompt in Claude buffer."
  (interactive)
  (let
      ((result
        (--emacs-claude-detect-prompt emacs-claude-prompt-y/y/n)))
    (when result
      t)))

;; Sub-functions
;; ------------------------------

(defun --emacs-claude-detect-prompt (prompt-text &optional n-lines)
  "Find prompt in current buffer using PROMPT-TEXT within N-LINES lines."
  (interactive)
  (let ((n-lines (or n-lines 10)))
    (when (get-buffer emacs-claude-buffer-name)
      (with-current-buffer emacs-claude-buffer-name
        (save-excursion
          (goto-char (point-max))
          (skip-chars-backward " \t\n\r")
          (let* ((current-line (line-number-at-pos))
                 (min-line (line-number-at-pos (point-min)))
                 (search-end-line
                  (max min-line (- current-line n-lines)))
                 (search-end (save-excursion
                               (goto-char (point-min))
                               (forward-line (1- search-end-line))
                               (point))))
            (let ((found (search-backward prompt-text search-end t)))
              found)))))))

(provide 'emacs-claude-auto-accept-detect-prompt)

(when
    (not load-file-name)
  (message "emacs-claude-auto-accept-detect-prompt.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))