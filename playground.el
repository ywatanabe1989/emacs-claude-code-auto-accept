;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 11:15:45>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/playground.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(ecc-buffer-new)
(ecc-buffer-next)
(ecc-buffer-prev)


(provide 'playground)

(when
    (not load-file-name)
  (message "playground.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))