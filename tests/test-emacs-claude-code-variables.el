;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:30:37>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/-test-emacs-claude-code-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'emacs-claude-code-variables)

(ert-deftest test-emacs-claude-code-variables-loadable ()
  (should (featurep 'emacs-claude-code-variables)))

(ert-deftest test-emacs-claude-code-variables-customization-group ()
  (should (get 'emacs-claude 'custom-group)))

(ert-deftest test-emacs-claude-buffer-name-defined ()
  (should (boundp 'emacs-claude-buffer-name))
  (should (stringp emacs-claude-buffer-name)))

(ert-deftest test-emacs-claude-buffer-defined ()
  (should (boundp 'emacs-claude-buffer)))

(ert-deftest test-emacs-claude-code-timer-defined ()
  (should (boundp 'emacs-claude-code-timer)))

(ert-deftest test-emacs-claude-prompt-patterns-defined ()
  (should (boundp 'emacs-claude-prompt-y/n))
  (should (stringp emacs-claude-prompt-y/n))
  (should (boundp 'emacs-claude-prompt-y/y/n))
  (should (stringp emacs-claude-prompt-y/y/n))
  (should (boundp 'emacs-claude-prompt-waiting))
  (should (stringp emacs-claude-prompt-waiting))
  (should (boundp 'emacs-claude-prompt-initial-waiting))
  (should (stringp emacs-claude-prompt-initial-waiting)))

(ert-deftest test-emacs-claude-code-interval-sec-defined ()
  (should (boundp 'emacs-claude-code-interval-sec))
  (should (numberp (if (stringp emacs-claude-code-interval-sec)
                       (string-to-number
                        emacs-claude-code-interval-sec)
                     emacs-claude-code-interval-sec))))


(provide '-test-emacs-claude-code-variables)

(when
    (not load-file-name)
  (message "-test-emacs-claude-code-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))