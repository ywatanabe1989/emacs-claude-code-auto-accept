;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:39:18>
;;; File: /home/ywatanabe/.emacs.d/lisp/ecc/tests/test-ecc-auto-enable-stop.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-auto-enable-stop)

(ert-deftest test-ecc-auto-enable-stop-loadable ()
  (should (featurep 'ecc-auto-enable-stop)))

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
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "vterm<123>")))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)
          (ecc-buffer-rename-buffer t)
          (should
           (string= (buffer-name mock-buffer) ecc-buffer-name)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer))))

(ert-deftest test-ecc-buffer-rename-buffer-when-disabled ()
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*CLAUDE-CODE*")))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)
          (cl-letf (((symbol-function 'emacs-pid) (lambda () 123)))
            (ecc-buffer-rename-buffer nil)
            (should (string= (buffer-name mock-buffer) "vterm<123>"))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer))))

(ert-deftest
    test-ecc-auto-enable-uses-current-buffer-when-no-buffer-exists
    ()
  (let ((orig-buffer ecc-buffer)
        (buffer-set nil))
    (unwind-protect
        (progn
          (setq ecc-buffer nil)
          (cl-letf
              (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
               ((symbol-function 'ecc-buffer-rename-buffer)
                #'ignore)
               ((symbol-function
                 'ecc-update-mode-line)
                #'ignore)
               ((symbol-function 'add-hook) #'ignore)
               ((symbol-function 'remove-hook) #'ignore)
               ((symbol-function 'run-with-timer)
                (lambda (&rest _) 'mock-timer))
               ((symbol-function 'vterm-send-key) #'ignore)
               ((symbol-function 'current-buffer)
                (lambda () 'current-buffer))
               ((symbol-function 'buffer-name)
                (lambda (&rest _) "buffer-name")))
            (ecc-auto-enable)
            (should (eq ecc-buffer 'current-buffer))))
      (setq ecc-buffer orig-buffer))))

(ert-deftest test-ecc-auto-enable-adds-hook-and-starts-timer
    ()
  (let ((orig-buffer ecc-buffer)
        (orig-timer ecc-timer)
        (hook-added nil)
        (timer-started nil))
    (unwind-protect
        (progn
          (setq ecc-buffer (current-buffer)
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
               ((symbol-function 'ecc-buffer-rename-buffer)
                #'ignore)
               ((symbol-function
                 'ecc-update-mode-line)
                #'ignore)
               ((symbol-function 'vterm-send-key) #'ignore))
            (ecc-auto-enable)
            (should (eq hook-added 'ecc-send-accept-))
            (should
             (eq timer-started 'ecc-auto-check-and-restart))
            (should (eq ecc-timer 'mock-timer))))
      (setq ecc-buffer orig-buffer
            ecc-timer orig-timer))))

(ert-deftest
    test-ecc-auto-disable-removes-hook-and-cancels-timer ()
  (let ((orig-buffer ecc-buffer)
        (orig-timer ecc-timer)
        (hook-removed nil)
        (timer-cancelled nil))
    (unwind-protect
        (progn
          (setq ecc-buffer (current-buffer)
                ecc-timer 'mock-timer)
          (cl-letf (((symbol-function 'remove-hook)
                     (lambda (hook function)
                       (when (and (eq hook 'vterm-update-functions)
                                  (eq function 'ecc-sendd-1-2-3-))
                         (setq hook-removed t))))
                    ((symbol-function 'cancel-timer)
                     (lambda (timer)
                       (when (eq timer 'mock-timer)
                         (setq timer-cancelled t))))
                    ((symbol-function 'ecc-buffer-rename-buffer)
                     #'ignore)
                    ((symbol-function
                      'ecc-update-mode-line)
                     #'ignore))
            (ecc-auto-disable)
            (should hook-removed)
            (should timer-cancelled)
            (should-not ecc-timer)))
      (setq ecc-buffer orig-buffer
            ecc-timer orig-timer))))

(ert-deftest
    test-ecc-auto-check-and-restart-adds-hook-when-missing ()
  (let ((orig-buffer ecc-buffer)
        (hook-added nil))
    (unwind-protect
        (progn
          (setq ecc-buffer (current-buffer))
          (cl-letf
              (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
               ((symbol-function 'member) (lambda (&rest _) nil))
               ((symbol-function 'add-hook)
                (lambda (hook function)
                  (when (eq hook 'vterm-update-functions)
                    (setq hook-added function))))
               ((symbol-function 'ecc-sendd-1-2-3-)
                #'ignore))
            (ecc-auto-check-and-restart)
            (should (eq hook-added 'ecc-sendd-1-2-3-))))
      (setq ecc-buffer orig-buffer))))

(ert-deftest
    test-ecc-auto-check-and-restart-finds-vterm-buffer-when-needed
    ()
  (let ((orig-buffer ecc-buffer)
        (found-buffer nil))
    (unwind-protect
        (progn
          (setq ecc-buffer nil)
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
                    ((symbol-function 'ecc-sendd-1-2-3-)
                     #'ignore)
                    ((symbol-function 'buffer-name)
                     (lambda (buf) "buffer-name")))
            (ecc-auto-check-and-restart)
            (should (eq ecc-buffer 'mock-vterm-buffer))))
      (setq ecc-buffer orig-buffer))))


(provide 'test-ecc-auto-enable-stop)

(when
    (not load-file-name)
  (message "test-ecc-auto-enable-stop.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))