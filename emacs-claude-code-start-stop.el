;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 04:13:45>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/emacs-claude-code-start-stop.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'emacs-claude-code-variables)
(require 'emacs-claude-code-send)
(require 'emacs-claude-code-update-mode-line)

;; External function declarations
(declare-function vterm-send-key "ext:vterm")
(declare-function derived-mode-p "subr")

(defvar vterm-update-functions)

(defun emacs-claude-code-toggle ()
  "Toggle auto-accepting Claude prompts."
  (interactive)
  (if (and emacs-claude-code-timer
           (member 'emacs-claude-code-send
                   vterm-update-functions))
      (emacs-claude-code-stop)
    (emacs-claude-code-start)))

(defun emacs-claude-code-rename-buffer (enable)
  "Rename buffer according to auto-accept status.
If ENABLE is non-nil, rename to special Claude name.
Otherwise, reset to a default vterm name."
  (when (buffer-live-p emacs-claude-buffer)
    (with-current-buffer emacs-claude-buffer
      (let ((current-name (buffer-name))
            (default-name
             (concat "vterm<" (number-to-string (emacs-pid)) ">")))
        (if enable
            (unless (string-match-p "\\*Claude.*\\*" current-name)
              (rename-buffer emacs-claude-buffer-name t))
          (when (string-match-p "\\*Claude.*\\*" current-name)
            (rename-buffer default-name t)))))))

(defun emacs-claude-code-start ()
  "Start auto-accepting Claude prompts using a hook and a timer."
  (interactive)
  (cond
   ;; 1. Check if buffer exists and is valid
   ((not (buffer-live-p emacs-claude-buffer))
    ;; 3. Check if current buffer is vterm-mode
    (if (derived-mode-p 'vterm-mode)
        ;; 4. Set current buffer as the claude buffer
        (setq emacs-claude-buffer (current-buffer))
      ;; 5. Not vterm-mode, raise message
      (message "Current buffer is not in vterm-mode")))
   ;; 2. Buffer exists but check if it's in vterm-mode
   ((with-current-buffer emacs-claude-buffer
      (not (derived-mode-p 'vterm-mode)))
    (message "Stored Claude buffer is not in vterm-mode")
    (setq emacs-claude-buffer nil)))
  ;; Continue if we have a valid vterm buffer
  (when (and (buffer-live-p emacs-claude-buffer)
             (with-current-buffer emacs-claude-buffer
               (derived-mode-p 'vterm-mode)))
    ;; Set up hook for immediate response to changes
    (remove-hook 'vterm-update-functions
                 'emacs-claude-code-send)
    (add-hook 'vterm-update-functions 'emacs-claude-code-send)
    ;; Also set up a timer for regular checking
    (when emacs-claude-code-timer
      (cancel-timer emacs-claude-code-timer))
    (setq emacs-claude-code-timer
          (run-with-timer 1 emacs-claude-code-interval-sec
                          'emacs-claude-code-check-and-restart))
    ;; Visual indicators
    (emacs-claude-code-rename-buffer t)
    (emacs-claude-code-update-mode-line t)
    (with-current-buffer emacs-claude-buffer
      (vterm-send-key "l" nil nil t))
    (message "Claude auto-accept enabled for buffer %s"
             (buffer-name emacs-claude-buffer))))

(defun emacs-claude-code-stop ()
  "Stop auto-accepting Claude prompts."
  (interactive)
  (remove-hook 'vterm-update-functions 'emacs-claude-code-send)
  (when emacs-claude-code-timer
    (cancel-timer emacs-claude-code-timer)
    (setq emacs-claude-code-timer nil))
  ;; Remove visual indicators
  (emacs-claude-code-rename-buffer nil)
  (emacs-claude-code-update-mode-line nil)
  (message "Claude auto-accept disabled"))

(defun emacs-claude-code-check-and-restart ()
  "Check if our hook is still active and run the accept function.
Also verify that the buffer is still valid and in vterm-mode."
  (condition-case nil
      (progn
        ;; Check if buffer is still valid
        (unless (and (buffer-live-p emacs-claude-buffer)
                     (with-current-buffer emacs-claude-buffer
                       (derived-mode-p 'vterm-mode)))
          ;; Try to find a new vterm buffer that might be Claude
          (let ((found-buffer nil))
            (dolist (buf (buffer-list))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (when (and (derived-mode-p 'vterm-mode)
                             (not found-buffer))
                    (setq found-buffer buf)))))
            (when found-buffer
              (setq emacs-claude-buffer found-buffer)
              (message "Claude auto-accept switched to buffer %s"
                       (buffer-name found-buffer)))))
        ;; Ensure the hook is still active
        (unless
            (member 'emacs-claude-code-send vterm-update-functions)
          (add-hook 'vterm-update-functions 'emacs-claude-code-send))
        ;; Run the accept function if we have a valid buffer
        (when (buffer-live-p emacs-claude-buffer)
          (emacs-claude-code-send)))
    (error nil)))


(provide 'emacs-claude-code-start-stop)

(when
    (not load-file-name)
  (message "emacs-claude-code-start-stop.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))