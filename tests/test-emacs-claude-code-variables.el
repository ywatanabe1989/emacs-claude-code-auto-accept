;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:12>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-emacs-claude-code-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-variables)

(ert-deftest test-ecc-variables-loadable ()
  (should (featurep 'ecc-variables)))

(ert-deftest test-ecc-variables-customization-group ()
  (should (get 'emacs-claude 'custom-group)))

(ert-deftest test-ecc-buffer-name-defined ()
  (should (boundp 'ecc-buffer-name))
  (should (stringp ecc-buffer-name)))

(ert-deftest test-ecc-buffer-defined ()
  (should (boundp 'ecc-buffer)))

(ert-deftest test-ecc-timer-defined ()
  (should (boundp 'ecc-timer)))

(ert-deftest test-ecc-prompt-patterns-defined ()
  (should (boundp 'ecc-prompt-y/n))
  (should (stringp ecc-prompt-y/n))
  (should (boundp 'ecc-prompt-y/y/n))
  (should (stringp ecc-prompt-y/y/n))
  (should (boundp 'ecc-prompt-waiting))
  (should (stringp ecc-prompt-waiting))
  (should (boundp 'ecc-prompt-initial-waiting))
  (should (stringp ecc-prompt-initial-waiting)))

(ert-deftest test-ecc-interval-sec-defined ()
  (should (boundp 'ecc-interval-sec))
  (should (numberp (if (stringp ecc-interval-sec)
                       (string-to-number
                        ecc-interval-sec)
                     ecc-interval-sec))))


(provide 'test-emacs-claude-code-variables)

(when
    (not load-file-name)
  (message "test-emacs-claude-code-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))