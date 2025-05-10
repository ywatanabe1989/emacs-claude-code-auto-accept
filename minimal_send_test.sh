#!/bin/bash
# A minimal test script that only tests ecc-send functions

cd "$(dirname "$0")"
echo "Running standalone tests for ecc-send..."
emacs -Q --batch --load './tests/standalone-test-ecc-send.el' --eval '(progn (mapatoms (lambda (sym) 
  (when (and 
         (fboundp sym) 
         (string-match "^test-ecc-" (symbol-name sym))
         (not (string-match "test-completion" (symbol-name sym))))
    (message "Running test %s" sym) 
    (funcall sym)))))'
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
  echo -e "\033[0;32mSUCCESS: All tests passed\033[0m"
else
  echo -e "\033[0;31mFAILURE: Some tests failed (exit code $EXIT_CODE)\033[0m"
fi