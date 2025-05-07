;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:35:51>
;;; File: /home/ywatanabe/.emacs.d/lisp/ecc/tests/test-ecc-update-mode-line.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-update-mode-line)

(ert-deftest test-ecc-update-mode-line-loadable ()
  (should (featurep 'ecc-update-mode-line)))

(ert-deftest test-ecc-mode-line-indicator-defined ()
  (should (boundp 'ecc-mode-line-indicator))
  (should (stringp ecc-mode-line-indicator)))

(ert-deftest test-ecc-update-mode-line-defined ()
  (should (fboundp 'ecc-update-mode-line)))

(ert-deftest test-ecc-update-mode-line-adds-indicator ()
  (let ((orig-global-mode-string global-mode-string))
    (unwind-protect
        (progn
          (setq global-mode-string nil)
          (ecc-update-mode-line t)
          (should
           (member ecc-mode-line-indicator
                   global-mode-string)))
      (setq global-mode-string orig-global-mode-string))))

(ert-deftest test-ecc-update-mode-line-removes-indicator
    ()
  (let ((orig-global-mode-string global-mode-string))
    (unwind-protect
        (progn
          (setq global-mode-string
                (list ecc-mode-line-indicator))
          (ecc-update-mode-line nil)
          (should-not
           (member ecc-mode-line-indicator
                   global-mode-string)))
      (setq global-mode-string orig-global-mode-string))))

(ert-deftest test-ecc-update-mode-line-adds-overlay ()
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (overlay-created nil))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)
          (cl-letf (((symbol-function 'make-overlay)
                     (lambda (&rest _) (setq overlay-created t)
                       'mock-overlay))
                    ((symbol-function 'overlay-put)
                     (lambda (&rest _) nil))
                    ((symbol-function 'delete-overlay)
                     (lambda (&rest _) nil)))
            (ecc-update-mode-line t)
            (should overlay-created)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer))))

(ert-deftest test-ecc-update-mode-line-removes-overlay
    ()
  (let ((orig-buffer ecc-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (overlay-deleted nil))
    (unwind-protect
        (progn
          (setq ecc-buffer mock-buffer)
          (with-current-buffer mock-buffer
            (setq-local ecc-buffer-name-overlay
                        'mock-overlay))
          (cl-letf (((symbol-function 'delete-overlay)
                     (lambda (overlay)
                       (when (eq overlay 'mock-overlay)
                         (setq overlay-deleted t)))))
            (ecc-update-mode-line nil)
            (should overlay-deleted)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq ecc-buffer orig-buffer))))

(ert-deftest test-ecc-update-mode-line-force-updates ()
  (let ((mode-line-updated nil))
    (cl-letf (((symbol-function 'force-mode-line-update)
               (lambda (&rest _) (setq mode-line-updated t))))
      (ecc-update-mode-line t)
      (should mode-line-updated))))


(provide 'test-ecc-update-mode-line)

(when
    (not load-file-name)
  (message "test-ecc-update-mode-line.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))