;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:39:18>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-emacs-claude-code-start-stop.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'emacs-claude-code-start-stop)

(ert-deftest test-emacs-claude-code-start-stop-loadable ()
  (should (featurep 'emacs-claude-code-start-stop)))

(ert-deftest test-emacs-claude-code-toggle-defined ()
  (should (fboundp 'emacs-claude-code-toggle)))

(ert-deftest test-emacs-claude-code-toggle-starts-when-inactive ()
  (let ((started nil)
        (stopped nil))
    (cl-letf
        (((symbol-function 'emacs-claude-code-timer) (lambda () nil))
         ((symbol-function 'emacs-claude-code-start)
          (lambda () (setq started t)))
         ((symbol-function 'emacs-claude-code-stop)
          (lambda () (setq stopped t)))
         ((symbol-function 'member) (lambda (&rest _) nil)))
      (emacs-claude-code-toggle)
      (should started)
      (should-not stopped))))

(ert-deftest test-emacs-claude-code-toggle-stops-when-active ()
  (let ((started nil)
        (stopped nil))
    (cl-letf
        (((symbol-function 'emacs-claude-code-timer) (lambda () t))
         ((symbol-function 'emacs-claude-code-start)
          (lambda () (setq started t)))
         ((symbol-function 'emacs-claude-code-stop)
          (lambda () (setq stopped t)))
         ((symbol-function 'member) (lambda (&rest _) t)))
      (emacs-claude-code-toggle)
      (should-not started)
      (should stopped))))

(ert-deftest test-emacs-claude-code-rename-buffer-when-enabled ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "vterm<123>")))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)
          (emacs-claude-code-rename-buffer t)
          (should
           (string= (buffer-name mock-buffer) emacs-claude-buffer-name)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest test-emacs-claude-code-rename-buffer-when-disabled ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*CLAUDE-CODE*")))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)
          (cl-letf (((symbol-function 'emacs-pid) (lambda () 123)))
            (emacs-claude-code-rename-buffer nil)
            (should (string= (buffer-name mock-buffer) "vterm<123>"))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest
    test-emacs-claude-code-start-uses-current-buffer-when-no-buffer-exists
    ()
  (let ((orig-buffer emacs-claude-buffer)
        (buffer-set nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer nil)
          (cl-letf
              (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
               ((symbol-function 'emacs-claude-code-rename-buffer)
                #'ignore)
               ((symbol-function
                 'emacs-claude-code-update-mode-line)
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
            (emacs-claude-code-start)
            (should (eq emacs-claude-buffer 'current-buffer))))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest test-emacs-claude-code-start-adds-hook-and-starts-timer
    ()
  (let ((orig-buffer emacs-claude-buffer)
        (orig-timer emacs-claude-code-timer)
        (hook-added nil)
        (timer-started nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer (current-buffer)
                emacs-claude-code-timer nil)
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
               ((symbol-function 'emacs-claude-code-rename-buffer)
                #'ignore)
               ((symbol-function
                 'emacs-claude-code-update-mode-line)
                #'ignore)
               ((symbol-function 'vterm-send-key) #'ignore))
            (emacs-claude-code-start)
            (should (eq hook-added 'emacs-claude-code-send))
            (should
             (eq timer-started 'emacs-claude-code-check-and-restart))
            (should (eq emacs-claude-code-timer 'mock-timer))))
      (setq emacs-claude-buffer orig-buffer
            emacs-claude-code-timer orig-timer))))

(ert-deftest
    test-emacs-claude-code-stop-removes-hook-and-cancels-timer ()
  (let ((orig-buffer emacs-claude-buffer)
        (orig-timer emacs-claude-code-timer)
        (hook-removed nil)
        (timer-cancelled nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer (current-buffer)
                emacs-claude-code-timer 'mock-timer)
          (cl-letf (((symbol-function 'remove-hook)
                     (lambda (hook function)
                       (when (and (eq hook 'vterm-update-functions)
                                  (eq function 'emacs-claude-code-send))
                         (setq hook-removed t))))
                    ((symbol-function 'cancel-timer)
                     (lambda (timer)
                       (when (eq timer 'mock-timer)
                         (setq timer-cancelled t))))
                    ((symbol-function 'emacs-claude-code-rename-buffer)
                     #'ignore)
                    ((symbol-function
                      'emacs-claude-code-update-mode-line)
                     #'ignore))
            (emacs-claude-code-stop)
            (should hook-removed)
            (should timer-cancelled)
            (should-not emacs-claude-code-timer)))
      (setq emacs-claude-buffer orig-buffer
            emacs-claude-code-timer orig-timer))))

(ert-deftest
    test-emacs-claude-code-check-and-restart-adds-hook-when-missing ()
  (let ((orig-buffer emacs-claude-buffer)
        (hook-added nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer (current-buffer))
          (cl-letf
              (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
               ((symbol-function 'member) (lambda (&rest _) nil))
               ((symbol-function 'add-hook)
                (lambda (hook function)
                  (when (eq hook 'vterm-update-functions)
                    (setq hook-added function))))
               ((symbol-function 'emacs-claude-code-send)
                #'ignore))
            (emacs-claude-code-check-and-restart)
            (should (eq hook-added 'emacs-claude-code-send))))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest
    test-emacs-claude-code-check-and-restart-finds-vterm-buffer-when-needed
    ()
  (let ((orig-buffer emacs-claude-buffer)
        (found-buffer nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer nil)
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
                    ((symbol-function 'emacs-claude-code-send)
                     #'ignore)
                    ((symbol-function 'buffer-name)
                     (lambda (buf) "buffer-name")))
            (emacs-claude-code-check-and-restart)
            (should (eq emacs-claude-buffer 'mock-vterm-buffer))))
      (setq emacs-claude-buffer orig-buffer))))


(provide 'test-emacs-claude-code-start-stop)

(when
    (not load-file-name)
  (message "test-emacs-claude-code-start-stop.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))