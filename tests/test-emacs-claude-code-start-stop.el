;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:10>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-emacs-claude-code-start-stop.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-auto)

(ert-deftest test-ecc-auto-loadable ()
  (should (featurep 'ecc-auto)))

(ert-deftest test-ecc-auto-toggle-defined ()
  (should (fboundp 'ecc-auto-toggle)))

(ert-deftest test-ecc-auto-toggle-starts-when-inactive ()
  (let ((started nil)
        (stopped nil))
    (cl-letf
        (((symbol-function 'ecc-timer) (lambda () nil))
         ((symbol-function 'ecc-auto-enable)
          (lambda () (setq started t)))
         ((symbol-function 'ecc-auto-disable)
          (lambda () (setq stopped t)))
         ((symbol-function 'member) (lambda (&rest _) nil)))
      (ecc-auto-toggle)
      (should started)
      (should-not stopped))))

(ert-deftest test-ecc-auto-toggle-stops-when-active ()
  (let ((started nil)
        (stopped nil))
    (cl-letf
        (((symbol-function 'ecc-timer) (lambda () t))
         ((symbol-function 'ecc-auto-enable)
          (lambda () (setq started t)))
         ((symbol-function 'ecc-auto-disable)
          (lambda () (setq stopped t)))
         ((symbol-function 'member) (lambda (&rest _) t)))
      (ecc-auto-toggle)
      (should-not started)
      (should stopped))))

(ert-deftest test-ecc-buffer-rename-buffer-when-enabled ()
  (let ((orig-buffer ecc-active-buffer)
        (mock-buffer (generate-new-buffer "vterm<123>"))
        (rename-called nil)
        (rename-args nil))
    (unwind-protect
        (progn
          (setq ecc-active-buffer mock-buffer)
          (cl-letf (((symbol-function 'rename-buffer)
                     (lambda (name &optional unique)
                       (setq rename-called t
                             rename-args (list name unique)))))
            (ecc-buffer-rename-buffer t)
            (should rename-called)
            (should (equal (car rename-args) ecc-buffer-name))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-active-buffer orig-buffer))))

(ert-deftest test-ecc-buffer-rename-buffer-when-disabled ()
  (let ((orig-buffer ecc-active-buffer)
        (mock-buffer (generate-new-buffer "*CLAUDE-CODE*"))
        (rename-called nil)
        (rename-args nil))
    (unwind-protect
        (progn
          (setq ecc-active-buffer mock-buffer)
          (cl-letf (((symbol-function 'emacs-pid) (lambda () 123))
                    ((symbol-function 'rename-buffer)
                     (lambda (name &optional unique)
                       (setq rename-called t
                             rename-args (list name unique)))))
            (ecc-buffer-rename-buffer nil)
            (should rename-called)
            (should (equal (car rename-args) "vterm<123>"))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-active-buffer orig-buffer))))

(ert-deftest test-ecc-auto-enable-uses-current-buffer-when-no-buffer-exists ()
  (let ((orig-active-buffer ecc-active-buffer)
        (buffer-set nil))
    (unwind-protect
        (progn
          (setq ecc-active-buffer nil)
          (cl-letf
              (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
               ((symbol-function 'ecc-buffer-rename-buffer) #'ignore)
               ((symbol-function 'ecc-update-mode-line) #'ignore)
               ((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore)
               ((symbol-function 'add-hook) #'ignore)
               ((symbol-function 'remove-hook) #'ignore)
               ((symbol-function 'run-with-timer)
                (lambda (&rest _) 'mock-timer))
               ((symbol-function 'vterm-send-key) #'ignore)
               ((symbol-function '--ecc-buffer-register-buffer)
                (lambda (buf) (setq ecc-active-buffer buf)))
               ((symbol-function 'current-buffer)
                (lambda () 'current-buffer))
               ((symbol-function 'buffer-name)
                (lambda (&rest _) "buffer-name")))
            (ecc-auto-enable)
            (should (eq ecc-active-buffer 'current-buffer))))
      (setq ecc-active-buffer orig-active-buffer))))

(ert-deftest test-ecc-auto-enable-adds-hook-and-starts-timer ()
  (let ((orig-active-buffer ecc-active-buffer)
        (orig-timer ecc-timer)
        (hook-added nil)
        (timer-started nil))
    (unwind-protect
        (progn
          (setq ecc-active-buffer (current-buffer)
                ecc-timer nil)
          (cl-letf
              (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
               ((symbol-function 'add-hook)
                (lambda (hook function)
                  (when (eq hook 'vterm-update-functions)
                    (setq hook-added function))))
               ((symbol-function 'run-with-timer)
                (lambda (secs repeat function)
                  (setq timer-started function)
                  'mock-timer))
               ((symbol-function 'remove-hook) #'ignore)
               ((symbol-function 'ecc-buffer-rename-buffer) #'ignore)
               ((symbol-function 'ecc-update-mode-line) #'ignore)
               ((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore)
               ((symbol-function 'vterm-send-key) #'ignore)
               ((symbol-function 'ecc-buffer-registry-cleanup-buffers) #'ignore))
            (ecc-auto-enable)
            (should (eq hook-added 'ecc-send-accept))
            (should (eq timer-started 'ecc-auto-check-and-restart))
            (should (eq ecc-timer 'mock-timer))))
      (setq ecc-active-buffer orig-active-buffer
            ecc-timer orig-timer))))

(ert-deftest test-ecc-auto-disable-removes-hook-and-cancels-timer ()
  (let ((orig-active-buffer ecc-active-buffer)
        (orig-timer ecc-timer)
        (hook-removed nil)
        (timer-cancelled nil))
    (unwind-protect
        (progn
          (setq ecc-active-buffer (current-buffer)
                ecc-timer 'mock-timer)
          (cl-letf (((symbol-function 'remove-hook)
                     (lambda (hook function)
                       (when (and (eq hook 'vterm-update-functions)
                                  (eq function 'ecc-send-accept))
                         (setq hook-removed t))))
                    ((symbol-function 'cancel-timer)
                     (lambda (timer)
                       (when (eq timer 'mock-timer)
                         (setq timer-cancelled t))))
                    ((symbol-function 'ecc-buffer-rename-buffer) #'ignore)
                    ((symbol-function 'ecc-update-mode-line) #'ignore))
            (ecc-auto-disable)
            (should hook-removed)
            (should timer-cancelled)
            (should-not ecc-timer)))
      (setq ecc-active-buffer orig-active-buffer
            ecc-timer orig-timer))))

(ert-deftest test-ecc-auto-check-and-restart-adds-hook-when-missing ()
  (let ((orig-active-buffer ecc-active-buffer)
        (hook-added nil))
    (unwind-protect
        (progn
          (setq ecc-active-buffer (current-buffer))
          (cl-letf
              (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
               ((symbol-function 'buffer-live-p) (lambda (&rest _) t))
               ((symbol-function 'member) (lambda (&rest _) nil))
               ((symbol-function 'add-hook)
                (lambda (hook function)
                  (when (eq hook 'vterm-update-functions)
                    (setq hook-added function))))
               ((symbol-function 'ecc-send-accept) #'ignore)
               ((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore))
            (ecc-auto-check-and-restart)
            (should (eq hook-added 'ecc-send-accept))))
      (setq ecc-active-buffer orig-active-buffer))))

(ert-deftest test-ecc-auto-check-and-restart-finds-vterm-buffer-when-needed ()
  (let ((orig-active-buffer ecc-active-buffer)
        (found-buffer nil))
    (unwind-protect
        (progn
          (setq ecc-active-buffer nil)
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda ()
                       (list 'buffer1 'buffer2 'mock-vterm-buffer)))
                    ((symbol-function 'derived-mode-p)
                     (lambda (mode)
                       (and (eq mode 'vterm-mode)
                            (eq (current-buffer) 'mock-vterm-buffer))))
                    ((symbol-function 'member) (lambda (&rest _) t))
                    ((symbol-function 'buffer-live-p) (lambda (buf) t))
                    ((symbol-function 'with-current-buffer)
                     (lambda (buffer body-function)
                       (let ((result (progn
                                       (setq current-buffer buffer)
                                       (funcall body-function))))
                         (setq current-buffer nil)
                         result)))
                    ((symbol-function 'ecc-send-accept) #'ignore)
                    ((symbol-function 'ecc-update-mode-line-all-buffers) #'ignore)
                    ((symbol-function '--ecc-buffer-register-buffer)
                     (lambda (buf) (setq ecc-active-buffer buf)))
                    ((symbol-function 'buffer-name)
                     (lambda (buf) "buffer-name")))
            (ecc-auto-check-and-restart)
            (should (eq ecc-active-buffer 'mock-vterm-buffer))))
      (setq ecc-active-buffer orig-active-buffer))))


(provide 'test-emacs-claude-code-start-stop)

(when
    (not load-file-name)
  (message "test-emacs-claude-code-start-stop.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))