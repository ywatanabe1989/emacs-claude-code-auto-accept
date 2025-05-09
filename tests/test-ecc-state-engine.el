;;; test-ecc-state-engine.el --- Tests for ecc-state-engine.el -*- lexical-binding: t -*-

(require 'ert)
(require 'test-setup)
(require 'ecc-state-engine)

;; Test state creation and registration
(ert-deftest test-ecc-state-engine-state-creation ()
  "Test state creation and properties."
  (let ((state (ecc-state--create :id 'test
                                :name "Test State"
                                :prompt-pattern "test-pattern"
                                :description "Test description"
                                :next-states '(idle running)
                                :action (lambda () (message "test")))))
    ;; Test basic properties
    (should (eq (ecc-state-id state) 'test))
    (should (string= (ecc-state-name state) "Test State"))
    (should (string= (ecc-state-prompt-pattern state) "test-pattern"))
    (should (string= (ecc-state-description state) "Test description"))
    (should (equal (ecc-state-next-states state) '(idle running)))
    (should (functionp (ecc-state-action state)))))

(ert-deftest test-ecc-state-engine-registration ()
  "Test state registration and retrieval."
  ;; Initialize to ensure clean state
  (ecc-state-engine-init)
  
  ;; Register a new test state
  (let ((state (ecc-state--create :id 'test-state
                                :name "Test State"
                                :prompt-pattern "test-pattern"
                                :description "Test description")))
    (ecc-state-engine-register-state state)
    
    ;; Verify it can be retrieved
    (let ((retrieved (ecc-state-engine-get-state 'test-state)))
      (should retrieved)
      (should (eq (ecc-state-id retrieved) 'test-state))
      (should (string= (ecc-state-name retrieved) "Test State")))))

;; Test state transitions
(ert-deftest test-ecc-state-engine-transitions ()
  "Test state transitions and current state tracking."
  ;; Initialize to ensure clean state
  (ecc-state-engine-init)
  
  ;; Verify initial state is idle
  (should (eq (ecc-state-engine-get-current-state-id) 'idle))
  
  ;; Transition to running state
  (ecc-state-engine-set-state 'running)
  (should (eq (ecc-state-engine-get-current-state-id) 'running))
  
  ;; Transition to waiting state
  (ecc-state-engine-set-state 'waiting)
  (should (eq (ecc-state-engine-get-current-state-id) 'waiting))
  
  ;; Transition back to idle
  (ecc-state-engine-set-state 'idle)
  (should (eq (ecc-state-engine-get-current-state-id) 'idle)))

;; Test state detection
(ert-deftest test-ecc-state-engine-detection ()
  "Test state detection from buffer content."
  ;; Create a test buffer with content that should trigger state detection
  (let ((buffer (generate-new-buffer "*claude-test*")))
    (unwind-protect
        (progn
          ;; Initialize to ensure clean state
          (ecc-state-engine-init)
          
          ;; Test running state detection
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Thinking...\nSome more content"))
          (ecc-state-engine-detect-state buffer)
          (should (eq (ecc-state-engine-get-current-state-id) 'running))
          
          ;; Test waiting state detection
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Press Enter to continue\nSome more content"))
          (ecc-state-engine-detect-state buffer)
          (should (eq (ecc-state-engine-get-current-state-id) 'waiting))
          
          ;; Test yes-no state detection
          (with-current-buffer buffer
            (erase-buffer)
            (insert "❯ 1. Yes\n❯ 2. No\nChoose an option"))
          (ecc-state-engine-detect-state buffer)
          (should (eq (ecc-state-engine-get-current-state-id) 'yes-no))
          
          ;; Test yes-yes-no state detection
          (with-current-buffer buffer
            (erase-buffer)
            (insert "❯ 1. Yes\n❯ 2. Yes, but with changes\n❯ 3. No\nChoose an option"))
          (ecc-state-engine-detect-state buffer)
          (should (eq (ecc-state-engine-get-current-state-id) 'yes-yes-no))
          
          ;; Test error state detection
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Error: Could not complete the request"))
          (ecc-state-engine-detect-state buffer)
          (should (eq (ecc-state-engine-get-current-state-id) 'error))
          
          ;; Test no pattern matches - should not change state
          (ecc-state-engine-set-state 'idle)
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Some random content with no patterns"))
          (ecc-state-engine-detect-state buffer)
          (should (eq (ecc-state-engine-get-current-state-id) 'idle)))
      
      ;; Clean up
      (kill-buffer buffer))))

;; Test hooks
(ert-deftest test-ecc-state-engine-hooks ()
  "Test state transition hooks."
  ;; Initialize to ensure clean state
  (ecc-state-engine-init)
  
  ;; Set up test hooks
  (let ((enter-hook-called nil)
        (exit-hook-called nil))
    
    ;; Add hooks
    (ecc-state-engine-add-hook 'idle 'exit (lambda () (setq exit-hook-called t)))
    (ecc-state-engine-add-hook 'running 'enter (lambda () (setq enter-hook-called t)))
    
    ;; Trigger transition
    (ecc-state-engine-set-state 'running)
    
    ;; Verify hooks were called
    (should enter-hook-called)
    (should exit-hook-called)
    
    ;; Reset flags
    (setq enter-hook-called nil)
    (setq exit-hook-called nil)
    
    ;; Remove hooks
    (ecc-state-engine-remove-hook 'idle 'exit (lambda () (setq exit-hook-called t)))
    (ecc-state-engine-remove-hook 'running 'enter (lambda () (setq enter-hook-called t)))
    
    ;; Transition back to idle
    (ecc-state-engine-set-state 'idle)
    
    ;; Verify hooks were not called
    (should-not enter-hook-called)
    (should-not exit-hook-called)))

;; Run the tests
(when (and (fboundp 'ert-run-tests-batch-and-exit)
           (not (member 'no-run-tests-automatically command-line-args)))
  (ert-run-tests-batch-and-exit (regexp-quote (symbol-name (quote ecc-state-engine)))))

(provide 'test-ecc-state-engine)
;;; test-ecc-state-engine.el ends here