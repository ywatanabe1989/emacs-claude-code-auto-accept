;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 02:16:45>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-claude-vterm-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-term/ecc-claude-vterm-mode)

;; Basic tests that don't require vterm
(ert-deftest test-ecc-claude-vterm-mode-loadable ()
  "Test that ecc-claude-vterm-mode can be loaded."
  (should (featurep 'ecc-term/ecc-claude-vterm-mode)))

(ert-deftest test-ecc-claude-vterm-mode-defined ()
  "Test that ecc-claude-vterm-mode is defined."
  (should (fboundp 'ecc-claude-vterm-mode)))

(ert-deftest test-ecc-claude-vterm-function-defined ()
  "Test that ecc-claude-vterm function is defined."
  (should (fboundp 'ecc-claude-vterm)))

(ert-deftest test-ecc-claude-vterm-customization-group-defined ()
  "Test that customization group is defined."
  (should (get 'ecc-claude-vterm 'custom-group)))

(ert-deftest test-ecc-claude-vterm-parent-mode-defined ()
  "Test that parent mode variable is defined."
  (should (boundp 'ecc-claude-vterm-parent-mode)))

(ert-deftest test-ecc-claude-vterm-mode-line-indicator-function ()
  "Test the mode line state indicator function."
  (cl-letf (((symbol-function 'ecc-state-get) (lambda () 'waiting)))
    (should (equal (ecc-claude-vterm-mode-line-state-indicator) " [Waiting]")))
  (cl-letf (((symbol-function 'ecc-state-get) (lambda () 'y/n)))
    (should (equal (ecc-claude-vterm-mode-line-state-indicator) " [Y/N]")))
  (cl-letf (((symbol-function 'ecc-state-get) (lambda () 'y/y/n)))
    (should (equal (ecc-claude-vterm-mode-line-state-indicator) " [Y/Y/N]"))))

(ert-deftest test-ecc-claude-vterm-mode-keymap-defined ()
  "Test that ecc-claude-vterm-mode-map is defined."
  (should (boundp 'ecc-claude-vterm-mode-map))
  (should (keymapp ecc-claude-vterm-mode-map)))

(ert-deftest test-ecc-claude-vterm-menu-defined ()
  "Test that ecc-claude-vterm-menu is defined."
  (should (boundp 'ecc-claude-vterm-menu))
  (should (keymapp ecc-claude-vterm-menu)))

(ert-deftest test-ecc-claude-vterm-interaction-functions-defined ()
  "Test that Claude vterm interaction functions are defined."
  (should (fboundp 'ecc-claude-vterm-interrupt))
  (should (fboundp 'ecc-claude-vterm-yes))
  (should (fboundp 'ecc-claude-vterm-no))
  (should (fboundp 'ecc-claude-vterm-retry))
  (should (fboundp 'ecc-claude-vterm-clear)))

;; Tests that require vterm - only run if available
(ert-deftest test-ecc-claude-vterm-mode-derives-from-vterm ()
  "Test that ecc-claude-vterm-mode is derived from vterm-mode if available."
  (skip-unless ecc-claude-vterm--vterm-available)
  (should (eq ecc-claude-vterm-parent-mode 'vterm-mode))
  (let ((mode-parent (get 'ecc-claude-vterm-mode 'derived-mode-parent)))
    (should (eq mode-parent 'vterm-mode))))

(ert-deftest test-ecc-claude-vterm-mode-sets-correct-variables ()
  "Test that ecc-claude-vterm-mode sets the correct variables."
  (skip-unless ecc-claude-vterm--vterm-available)
  (let ((test-buffer (generate-new-buffer "*test-ecc-claude-vterm*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (let ((vterm-shell "/bin/echo"))
            (ecc-claude-vterm-mode)
            (should (eq major-mode 'ecc-claude-vterm-mode))
            (should (eq scroll-conservatively ecc-claude-vterm-scroll-conservatively))
            (should (eq truncate-lines ecc-claude-vterm-truncate-lines))))
      (kill-buffer test-buffer))))

(ert-deftest test-ecc-claude-vterm-falls-back-to-special-mode ()
  "Test that ecc-claude-vterm-mode falls back to special-mode if vterm isn't available."
  (unless ecc-claude-vterm--vterm-available
    (should (eq ecc-claude-vterm-parent-mode 'special-mode))))

(ert-deftest test-ecc-claude-vterm-create-buffer-errors-without-vterm ()
  "Test that creating a Claude vterm buffer errors without vterm."
  (unless ecc-claude-vterm--vterm-available
    (should-error (ecc-claude-vterm) :type 'error)))

(provide 'test-ecc-claude-vterm-mode)

(when (not load-file-name)
  (message "test-ecc-claude-vterm-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))