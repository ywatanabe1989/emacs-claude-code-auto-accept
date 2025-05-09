;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-08 15:45:20>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-buffer/ecc-buffer)
(require 'ecc-buffer/ecc-buffer-variables)
(require 'ecc-buffer/ecc-buffer-verification)
(require 'ecc-buffer/ecc-buffer-registry)
(require 'ecc-buffer/ecc-buffer-current)
(require 'ecc-buffer/ecc-buffer-state)
(require 'ecc-buffer/ecc-buffer-stale)
(require 'ecc-buffer/ecc-buffer-navigation)
(require 'ecc-buffer/ecc-buffer-timestamp)

(ert-deftest test-ecc-buffer-loadable ()
  "Test that ecc-buffer loads properly."
  (should (featurep 'ecc-buffer/ecc-buffer)))

(ert-deftest test-ecc-buffer-variables-required ()
  "Test that ecc-buffer-variables is required."
  (should (featurep 'ecc-buffer/ecc-buffer-variables)))

(ert-deftest test-ecc-buffer-verification-required ()
  "Test that ecc-buffer-verification is required."
  (should (featurep 'ecc-buffer/ecc-buffer-verification)))

(ert-deftest test-ecc-buffer-registry-required ()
  "Test that ecc-buffer-registry is required."
  (should (featurep 'ecc-buffer/ecc-buffer-registry)))

(ert-deftest test-ecc-buffer-current-required ()
  "Test that ecc-buffer-current is required."
  (should (featurep 'ecc-buffer/ecc-buffer-current)))

(ert-deftest test-ecc-buffer-state-required ()
  "Test that ecc-buffer-state is required."
  (should (featurep 'ecc-buffer/ecc-buffer-state)))

(ert-deftest test-ecc-buffer-stale-required ()
  "Test that ecc-buffer-stale is required."
  (should (featurep 'ecc-buffer/ecc-buffer-stale)))

(ert-deftest test-ecc-buffer-navigation-required ()
  "Test that ecc-buffer-navigation is required."
  (should (featurep 'ecc-buffer/ecc-buffer-navigation)))

(ert-deftest test-ecc-buffer-timestamp-required ()
  "Test that ecc-buffer-timestamp is required."
  (should (featurep 'ecc-buffer/ecc-buffer-timestamp)))

(provide 'test-ecc-buffer)

(when
    (not load-file-name)
  (message "test-ecc-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))