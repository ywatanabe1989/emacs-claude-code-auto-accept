;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:35:51>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-emacs-claude-code-update-mode-line.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'emacs-claude-code-update-mode-line)

(ert-deftest test-emacs-claude-code-update-mode-line-loadable ()
  (should (featurep 'emacs-claude-code-update-mode-line)))

(ert-deftest test-emacs-claude-code-mode-line-indicator-defined ()
  (should (boundp 'emacs-claude-code-mode-line-indicator))
  (should (stringp emacs-claude-code-mode-line-indicator)))

(ert-deftest test-emacs-claude-code-update-mode-line-defined ()
  (should (fboundp 'emacs-claude-code-update-mode-line)))

(ert-deftest test-emacs-claude-code-update-mode-line-adds-indicator ()
  (let ((orig-global-mode-string global-mode-string))
    (unwind-protect
        (progn
          (setq global-mode-string nil)
          (emacs-claude-code-update-mode-line t)
          (should
           (member emacs-claude-code-mode-line-indicator
                   global-mode-string)))
      (setq global-mode-string orig-global-mode-string))))

(ert-deftest test-emacs-claude-code-update-mode-line-removes-indicator
    ()
  (let ((orig-global-mode-string global-mode-string))
    (unwind-protect
        (progn
          (setq global-mode-string
                (list emacs-claude-code-mode-line-indicator))
          (emacs-claude-code-update-mode-line nil)
          (should-not
           (member emacs-claude-code-mode-line-indicator
                   global-mode-string)))
      (setq global-mode-string orig-global-mode-string))))

(ert-deftest test-emacs-claude-code-update-mode-line-adds-overlay ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (overlay-created nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)
          (cl-letf (((symbol-function 'make-overlay)
                     (lambda (&rest _) (setq overlay-created t)
                       'mock-overlay))
                    ((symbol-function 'overlay-put)
                     (lambda (&rest _) nil))
                    ((symbol-function 'delete-overlay)
                     (lambda (&rest _) nil)))
            (emacs-claude-code-update-mode-line t)
            (should overlay-created)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest test-emacs-claude-code-update-mode-line-removes-overlay
    ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (overlay-deleted nil))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer mock-buffer)
          (with-current-buffer mock-buffer
            (setq-local emacs-claude-code-buffer-name-overlay
                        'mock-overlay))
          (cl-letf (((symbol-function 'delete-overlay)
                     (lambda (overlay)
                       (when (eq overlay 'mock-overlay)
                         (setq overlay-deleted t)))))
            (emacs-claude-code-update-mode-line nil)
            (should overlay-deleted)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest test-emacs-claude-code-update-mode-line-force-updates ()
  (let ((mode-line-updated nil))
    (cl-letf (((symbol-function 'force-mode-line-update)
               (lambda (&rest _) (setq mode-line-updated t))))
      (emacs-claude-code-update-mode-line t)
      (should mode-line-updated))))


(provide 'test-emacs-claude-code-update-mode-line)

(when
    (not load-file-name)
  (message "test-emacs-claude-code-update-mode-line.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))