;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: root
;;; Timestamp: <2025-05-05 00:02:54>
;;; File: /root/.emacs.d/lisp/emacs-claude-code-auto-accept/emacs-claude-auto-accept-detect-prompt.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;; Detectors
;; ------------------------------

(defun --emacs-claude-detect-prompt-initial-waiting ()
  "Detect y/n prompt in Claude buffer."
  (--emacs-claude-detect-prompt emacs-claude-prompt-initial-waiting))

(defun --emacs-claude-detect-prompt-waiting ()
  "Detect waiting prompt in Claude buffer."
  (--emacs-claude-detect-prompt emacs-claude-prompt-waiting))

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
  "Find prompt in current buffer using PROMPT-TEXT within N-LINES lines."
  (interactive)
  (let ((n-lines (or n-lines 50)))
    (if (get-buffer emacs-claude-buffer-name)
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
              (let
                  ((found (search-backward prompt-text search-end t)))
                found))))
      nil)))

;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: root
;; ;;; Timestamp: <2025-05-04 23:58:55>
;; ;;; File: /root/.emacs.d/lisp/emacs-claude-code-auto-accept/emacs-claude-auto-accept-detect-prompt.el

;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; ;; Detectors
;; ;; ------------------------------

;; (defun --emacs-claude-detect-prompt-initial-waiting ()
;;   "Detect y/n prompt in Claude buffer."
;;   (--emacs-claude-detect-prompt emacs-claude-prompt-initial-waiting))

;; (defun --emacs-claude-detect-prompt-waiting ()
;;   "Detect waiting prompt in Claude buffer."
;;   (--emacs-claude-detect-prompt emacs-claude-prompt-waiting))

;; (defun --emacs-claude-detect-prompt-y/n ()
;;   "Detect y/n prompt in Claude buffer."
;;   (and
;;    (--emacs-claude-detect-prompt emacs-claude-prompt-y/n)
;;    (not (--emacs-claude-detect-prompt-y/y/n))))

;; (defun --emacs-claude-detect-prompt-y/y/n ()
;;   "Detect y/n prompt in Claude buffer."
;;   (--emacs-claude-detect-prompt emacs-claude-prompt-y/y/n))

;; ;; Sub-functions
;; ;; ------------------------------

;; (defun --emacs-claude-detect-prompt (prompt-text &optional n-lines)
;;   "Find prompt in current buffer using PROMPT-TEXT within N-LINES lines."
;;   (interactive)
;;   (let ((n-lines (or n-lines 50)))
;;     (if (get-buffer emacs-claude-buffer-name)
;;         (with-current-buffer emacs-claude-buffer-name
;;           (save-excursion
;;             (goto-char (point-max))
;;             (skip-chars-backward " \t\n\r")
;;             (let* ((current-line (line-number-at-pos))
;;                    (min-line (line-number-at-pos (point-min)))
;;                    (search-end-line
;;                     (max min-line (- current-line n-lines)))
;;                    (search-end (save-excursion
;;                                  (goto-char (point-min))
;;                                  (forward-line (1- search-end-line))
;;                                  (point))))
;;               (let
;;                   ((found (search-backward prompt-text search-end t)))
;;                 (message "Search result: %s"
;;                          (if found (format "Found at %d" found)
;;                            "Not found"))
;;                 found))))
;;       (message "No buffer named %s found" emacs-claude-buffer-name)
;;       nil)))

;; (provide 'emacs-claude-auto-accept-detect-prompt)

;; (when
;;     (not load-file-name)
;;   (message "emacs-claude-auto-accept-detect-prompt.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))

(provide 'emacs-claude-auto-accept-detect-prompt)

(when
    (not load-file-name)
  (message "emacs-claude-auto-accept-detect-prompt.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))