;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 03:35:08>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code-detect-prompt.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'pulse)

;; Detectors
;; ------------------------------

(defun --emacs-claude-detect-prompt-waiting ()
  "Detect waiting prompt in Claude buffer."
  (--emacs-claude-detect-prompt emacs-claude-prompt-waiting))

(defun --emacs-claude-detect-prompt-initial-waiting ()
  "Detect y/n prompt in Claude buffer."
  (--emacs-claude-detect-prompt emacs-claude-prompt-initial-waiting))

(defun --emacs-claude-detect-prompt-y/n ()
  "Detect y/n prompt in Claude buffer."
  (and
   (--emacs-claude-detect-prompt emacs-claude-prompt-y/n)
   (not (--emacs-claude-detect-prompt-y/y/n))))

(defun --emacs-claude-detect-prompt-y/y/n ()
  "Detect y/n prompt in Claude buffer."
  (--emacs-claude-detect-prompt emacs-claude-prompt-y/y/n))

;; Sub-functions
;; ------------------------------

(defun --emacs-claude-detect-prompt (prompt-text &optional n-lines)
  "Find prompt in Claude buffer using PROMPT-TEXT within N-LINES lines."
  (interactive)
  (let ((n-lines (or n-lines 50)))
    (if (buffer-live-p emacs-claude-buffer)
        (with-current-buffer emacs-claude-buffer
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
              (let
                  ((found (search-backward prompt-text search-end t)))
                (when found
                  (vterm-clear)
                  (condition-case nil
                      (save-excursion
                        (goto-char found)
                        (beginning-of-line)
                        (let ((line-start (point)))
                          (end-of-line)
                          (pulse-momentary-highlight-region line-start
                                                            (point))))
                    (error nil)))
                found))))
      nil)))


(provide 'emacs-claude-code-detect-prompt)

(when
    (not load-file-name)
  (message "emacs-claude-code-detect-prompt.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))