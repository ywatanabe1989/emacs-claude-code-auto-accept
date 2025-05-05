;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:35:10>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-emacs-claude-code-detect-prompt.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'emacs-claude-code-detect-prompt)

(ert-deftest test-emacs-claude-code-detect-prompt-loadable ()
  (should (featurep 'emacs-claude-code-detect-prompt)))

(ert-deftest test-emacs-claude-detect-prompt-waiting-defined ()
  (should (fboundp '--emacs-claude-detect-prompt-waiting)))

(ert-deftest test-emacs-claude-detect-prompt-initial-waiting-defined
    ()
  (should (fboundp '--emacs-claude-detect-prompt-initial-waiting)))

(ert-deftest test-emacs-claude-detect-prompt-y/n-defined ()
  (should (fboundp '--emacs-claude-detect-prompt-y/n)))

(ert-deftest test-emacs-claude-detect-prompt-y/y/n-defined ()
  (should (fboundp '--emacs-claude-detect-prompt-y/y/n)))

(ert-deftest test-emacs-claude-detect-prompt-defined ()
  (should (fboundp '--emacs-claude-detect-prompt)))

(ert-deftest test-emacs-claude-detect-prompt-with-mock-buffer ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*"))
        (test-prompt "❯ 1. Yes"))
    (unwind-protect
        (progn
          (with-current-buffer mock-buffer
            (insert "Some content before the prompt\n" test-prompt
                    "\nSome content after"))
          (setq emacs-claude-buffer mock-buffer)
          (should (--emacs-claude-detect-prompt test-prompt)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest test-emacs-claude-detect-prompt-handles-nil-buffer ()
  (let ((orig-buffer emacs-claude-buffer))
    (unwind-protect
        (progn
          (setq emacs-claude-buffer nil)
          (should-not (--emacs-claude-detect-prompt "any prompt")))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest
    test-emacs-claude-detect-prompt-returns-nil-when-not-found ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*")))
    (unwind-protect
        (progn
          (with-current-buffer mock-buffer
            (insert "Content without the target prompt"))
          (setq emacs-claude-buffer mock-buffer)
          (should-not
           (--emacs-claude-detect-prompt "Target prompt not present")))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))

(ert-deftest test-emacs-claude-y/n-prompt-detection ()
  (let ((orig-buffer emacs-claude-buffer)
        (mock-buffer (generate-new-buffer "*MOCK-CLAUDE*")))
    (unwind-protect
        (progn
          (with-current-buffer mock-buffer
            (insert "Some content\n❯ 1. Yes\nMore content"))
          (setq emacs-claude-buffer mock-buffer)
          (should (--emacs-claude-detect-prompt-y/n)))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer))
      (setq emacs-claude-buffer orig-buffer))))


(provide 'test-emacs-claude-code-detect-prompt)

(when
    (not load-file-name)
  (message "test-emacs-claude-code-detect-prompt.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))