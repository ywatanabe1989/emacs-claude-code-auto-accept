;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-07 12:27:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/ecc-templates.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defcustom ecc-templates-directory
  (expand-file-name "templates"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory containing template files for Claude responses."
  :type 'directory
  :group 'emacs-claude)

(defvar ecc-template-cache (make-hash-table :test 'equal)
  "Cache for loaded templates to avoid repeated disk reads.")

(defun ecc-template-load (template-name)
  "Load template content from TEMPLATE-NAME file in templates directory."
  (let* ((template-path (expand-file-name
                         (concat template-name
                                 (unless
                                     (string-match-p "\\.md$"
                                                     template-name)
                                   ".md"))
                         ecc-templates-directory))
         (cached (gethash template-path ecc-template-cache)))
    (if cached
        cached
      (if (file-exists-p template-path)
          (with-temp-buffer
            (insert-file-contents template-path)
            (let ((content (buffer-substring-no-properties
                            (point-min) (point-max))))
              (setq content (replace-regexp-in-string
                             "<!--[^>]*-->" "" content))
              (setq content (string-trim content))
              (puthash template-path content ecc-template-cache)
              content))
        (message "Template file not found: %s" template-path)
        nil))))

(defun ecc-template-list ()
  "Return a list of available template names."
  (when (file-directory-p ecc-templates-directory)
    (mapcar #'file-name-sans-extension
            (directory-files ecc-templates-directory nil "\\.md$"))))

(defun ecc-send-template ()
  "Interactively select and send a template response for Y/Y/N situations."
  (interactive)
  (let* ((template-list (ecc-template-list))
         (template-name (completing-read "Select template: "
                                         (append template-list
                                                 '("Custom..."))))
         (template-text
          (if (string= template-name "Custom...")
              (read-string "Enter custom response: ")
            (ecc-template-load template-name))))
    (when template-text
      (--ecc-auto-send-template template-text))))

(defun --ecc-auto-send-continue ()
  "Automatically respond with continue to Claude waiting prompts."
  (interactive)
  (--ecc-auto-send-by-state
   ecc-prompt-for-waiting
   (lambda ()
     (or (--ecc-state-waiting-p)
         (--ecc-state-initial-waiting-p)))))

(defun ecc-template-create (template-name content)
  "Create a new template with TEMPLATE-NAME and CONTENT."
  (interactive "sTemplate name: \nsTemplate content: ")
  (let* ((template-path (expand-file-name
                         (concat template-name
                                 (unless
                                     (string-match-p "\\.md$"
                                                     template-name)
                                   ".md"))
                         ecc-templates-directory)))
    (unless (file-directory-p ecc-templates-directory)
      (make-directory ecc-templates-directory t))
    (with-temp-file template-path
      (insert "<!-- ---\n")
      (insert (format "-- Timestamp: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "-- Author: %s\n" user-login-name))
      (insert (format "-- File: %s\n" template-path))
      (insert "-- --- -->\n")
      (insert content)
      (insert "\n<!-- EOF -->"))
    (puthash template-path content ecc-template-cache)
    (message "Template created: %s" template-path)))

(defun ecc-template-edit (template-name)
  "Edit an existing template with TEMPLATE-NAME."
  (interactive
   (list (completing-read "Edit template: " (ecc-template-list))))
  (let ((template-path (expand-file-name
                        (concat template-name
                                (unless
                                    (string-match-p "\\.md$"
                                                    template-name)
                                  ".md"))
                        ecc-templates-directory)))
    (if (file-exists-p template-path)
        (find-file template-path)
      (message "Template file not found: %s" template-path))))

;; ;; Implement ecc functions equivalent for these genai functions
;; (cl-defun genai--select-template ()
;;   "Prompt the user to select a template type for the GenAI model."
;;   (unless (minibufferp)
;;     (let ((capital-templates (genai--fetch-templates genai-templates-dir))
;;           (shortcuts (make-hash-table :test 'equal))
;;           (key-count (make-hash-table :test 'equal))
;;           (prompt-parts nil))

;;       ;; Handle mapped templates first
;;       (when (boundp 'genai-template-mapping)
;;         (dolist (mapping genai-template-mapping)
;;           (let* ((key (car mapping))
;;                  (value (cdr mapping))
;;                  (base-key (substring key 0 1))
;;                  (count (gethash base-key key-count 0)))
;;             (when (member value capital-templates)
;;               (puthash base-key (1+ count) key-count)
;;               (puthash key value shortcuts)
;;               (push (format "(%s) %s" key value) prompt-parts)))))

;;       ;; Handle unmapped templates
;;       (dolist (template capital-templates)
;;         (unless (rassoc template genai-template-mapping)
;;           (let* ((base-key (downcase (substring template 0 1)))
;;                  (count (gethash base-key key-count 0))
;;                  (key (if (> count 0)
;;                           (format "%s%d" base-key (1+ count))
;;                         base-key)))
;;             (puthash base-key (1+ count) key-count)
;;             (puthash key template shortcuts)
;;             (push (format "(%s) %s" key template) prompt-parts))))

;;       (setq prompt-parts (sort prompt-parts 'string<))
;;       (let* ((prompt (concat "Template or Manual Instruction:\n"
;;                             (mapconcat 'identity prompt-parts " ")
;;                             "\n"))
;;              (input (read-string prompt))
;;              (template-type (or (gethash input shortcuts)
;;                                (if (string-blank-p input)
;;                                    "None"
;;                                  input))))
;;         (unless (string= input "r")
;;           (display-buffer (get-buffer-create genai-buffer-name)))
;;         template-type))))

;; (defun genai-on-region ()
;;   "Run GenAI on region, dired or prompt."
;;   (interactive)
;;   (genai-interactive-mode 1)
;;   (let* ((marked-files
;;           (and (eq major-mode 'dired-mode)
;;                (condition-case nil
;;                   (dired-get-marked-files nil nil)
;;                   (user-error nil))))
;;          (prompt-text
;;           (cond
;;            ((< 1 (length marked-files))
;;             (genai-interactive-mode -1)
;;             (genai-on-region-list-files)
;;             nil)
;;            ((use-region-p)
;;             (prog1
;;                 (buffer-substring-no-properties
;;                  (region-beginning) (region-end))
;;               (deactivate-mark)))
;;            (t
;;             (read-string "Enter prompt: " "")))))

;;     (cond
;;      ;; Special command shortcuts
;;      ((equal prompt-text "g")
;;       (genai-interactive-mode -1)
;;       (switch-to-buffer-other-window genai-buffer-name)
;;       (keyboard-quit)
;;       (message "Jumped to *GenAI*"))

;;      ((equal prompt-text "h")
;;       (genai-interactive-mode -1)
;;       (genai-show-history)
;;       (keyboard-quit)
;;       (message "Showing history"))

;;      ;; Normal prompt handling
;;      (prompt-text
;;       (let ((template-type (genai--select-template)))
;;         (when template-type
;;           (genai-interactive-mode -1)
;;           (genai--ensure-dependencies)
;;           (genai--run-with-template prompt-text template-type)))))))


(provide 'ecc-templates)

(when
    (not load-file-name)
  (message "ecc-templates.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))