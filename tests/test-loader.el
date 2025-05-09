;;; test-loader.el --- Test loader for emacs-claude-code

;; Set up load-path for tests
(let* ((this-file (or load-file-name buffer-file-name))
       (tests-dir (file-name-directory this-file))
       (root-dir (expand-file-name ".." tests-dir))
       (src-dir (expand-file-name "src" root-dir))
       (modules-dir (expand-file-name "modules" tests-dir)))
  
  ;; Add all necessary directories to load-path
  (add-to-list 'load-path root-dir)
  (add-to-list 'load-path src-dir)
  (add-to-list 'load-path tests-dir)
  (add-to-list 'load-path modules-dir)
  
  ;; Add all src subdirectories to load-path
  (dolist (dir (directory-files src-dir t "\\`[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;; Load compatibility layers
(require 'test-compat)
(require 'fix-names)

(provide 'test-loader)