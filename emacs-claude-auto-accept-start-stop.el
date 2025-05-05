;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-05 11:18:52>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code-auto-accept/emacs-claude-auto-accept-start-stop.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defun emacs-claude-auto-accept-start ()
  "Start auto-accepting Claude prompts using a hook and a timer."
  (interactive)
  (cond
   ;; 1. Check if buffer exists
   ((not (get-buffer emacs-claude-buffer-name))
    ;; 3. Check if current buffer is vterm-mode
    (if (derived-mode-p 'vterm-mode)
        ;; 4. Rename current buffer to emacs-claude-buffer-name
        (rename-buffer emacs-claude-buffer-name t)
      ;; 5. Not vterm-mode, raise message
      (message "Current buffer is not in vterm-mode")))
   ;; 2. Buffer exists but check if it's in vterm-mode
   ((with-current-buffer emacs-claude-buffer-name
      (not (derived-mode-p 'vterm-mode)))
    (message "Buffer %s is not in vterm-mode" emacs-claude-buffer-name)))

  ;; Continue if we have a valid vterm buffer
  (when (and (get-buffer emacs-claude-buffer-name)
             (with-current-buffer emacs-claude-buffer-name
               (derived-mode-p 'vterm-mode)))
    ;; Set up hook for immediate response to changes
    (add-hook 'vterm-update-functions 'emacs-claude-auto-accept-send)
    ;; Also set up a timer for regular checking
    (when emacs-claude-auto-accept-timer
      (cancel-timer emacs-claude-auto-accept-timer))
    (setq emacs-claude-auto-accept-timer
          (run-with-timer 1 emacs-claude-auto-accept-interval-sec
                          'emacs-claude-auto-accept-send))
    (with-current-buffer emacs-claude-buffer-name
      (vterm-send-key "l" nil nil t))
    (message "Claude auto-accept enabled for %s"
             emacs-claude-buffer-name)))

(defun emacs-claude-auto-accept-stop ()
  "Stop auto-accepting Claude prompts."
  (interactive)
  (remove-hook 'vterm-update-functions 'emacs-claude-auto-accept-send)

  (when emacs-claude-auto-accept-timer
    (cancel-timer emacs-claude-auto-accept-timer)
    (setq emacs-claude-auto-accept-timer nil))

  (message "Claude auto-accept disabled"))


(provide 'emacs-claude-auto-accept-start-stop)

(when
    (not load-file-name)
  (message "emacs-claude-auto-accept-start-stop.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))