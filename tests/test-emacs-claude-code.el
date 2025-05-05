;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:37:34>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-emacs-claude-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'emacs-claude-code)

(ert-deftest test-emacs-claude-code-loadable ()
  (should (featurep 'emacs-claude-code)))

(ert-deftest test-emacs-claude-code-variables-required ()
  (should (featurep 'emacs-claude-code-variables)))

(ert-deftest test-emacs-claude-code-detect-prompt-required ()
  (should (featurep 'emacs-claude-code-detect-prompt)))

(ert-deftest test-emacs-claude-code-send-required ()
  (should (featurep 'emacs-claude-code-send)))

(ert-deftest test-emacs-claude-code-update-mode-line-required ()
  (should (featurep 'emacs-claude-code-update-mode-line)))

(ert-deftest test-emacs-claude-code-start-stop-required ()
  (should (featurep 'emacs-claude-code-start-stop)))

(ert-deftest test-emacs-claude-code-run-required ()
  (should (featurep 'emacs-claude-code-run)))

(ert-deftest test-emacs-claude-code-copy-repository-required ()
  (should (featurep 'emacs-claude-code-copy-repository)))


(provide 'test-emacs-claude-code)

(when
    (not load-file-name)
  (message "test-emacs-claude-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))