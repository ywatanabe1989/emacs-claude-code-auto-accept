;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:37:34>
;;; File: /home/ywatanabe/.emacs.d/lisp/ecc/tests/test-ecc.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc)

(ert-deftest test-ecc-loadable ()
  (should (featurep 'ecc)))

(ert-deftest test-ecc-variables-required ()
  (should (featurep 'ecc-variables)))

(ert-deftest test-ecc-detect-prompt-required ()
  (should (featurep 'ecc-detect-prompt)))

(ert-deftest test-ecc-send-required ()
  (should (featurep 'ecc-send-accept)))

(ert-deftest test-ecc-update-mode-line-required ()
  (should (featurep 'ecc-update-mode-line)))

(ert-deftest test-ecc-auto-enable-stop-required ()
  (should (featurep 'ecc-auto-enable-stop)))

(ert-deftest test-ecc-run-required ()
  (should (featurep 'ecc-run)))

(ert-deftest test-ecc-repository-copy-contents-required ()
  (should (featurep 'ecc-repository-copy-contents)))


(provide 'test-ecc)

(when
    (not load-file-name)
  (message "test-ecc.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))