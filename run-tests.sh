#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-07 12:27:40 (ywatanabe)"
# File: ./run-tests.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
touch "$LOG_PATH" >/dev/null 2>&1


# Set base directory for the project
BASE_DIR="$(dirname "$(readlink -f "$0")")"
SRC_DIR="$BASE_DIR"
TEST_DIR="$BASE_DIR/tests"

# Check if Emacs is available
if ! command -v emacs &> /dev/null; then
    echo "Error: Emacs not found. Please install Emacs."
    exit 1
fi

# Create a temporary elisp file with mock definitions
MOCK_FILE="$(mktemp)"
cat > "$MOCK_FILE" << 'EOF'
;; Mock external dependencies
(unless (fboundp 'vterm-mode)
  (defun vterm-mode ()))
(unless (fboundp 'vterm-send-string)
  (defun vterm-send-string (str)))
(unless (fboundp 'vterm-send-return)
  (defun vterm-send-return ()))
(unless (fboundp 'vterm-clear)
  (defun vterm-clear ()))
(unless (fboundp 'vterm-copy-mode)
  (defun vterm-copy-mode (arg)))
(unless (fboundp 'vterm-send-key)
  (defun vterm-send-key (key &optional mod1 mod2 mod3)))
(unless (boundp 'vterm-update-functions)
  (defvar vterm-update-functions nil))
(unless (fboundp 'magit-toplevel)
  (defun magit-toplevel (&optional directory)
    (or directory default-directory)))
EOF

# Print header
echo "====================================="
echo "Running tests for ecc"
echo "====================================="

# Run a specific test if provided, otherwise run all tests
if [ -n "$1" ]; then
    TEST_PATTERN="$1"
    echo "Running test pattern: $TEST_PATTERN"
else
    TEST_PATTERN="test-ecc"
    echo "Running all tests"
fi

# Run the tests
emacs --batch \
    -L "$SRC_DIR" \
    -L "$TEST_DIR" \
    -l ert \
    -l "$MOCK_FILE" \
    -l "$TEST_DIR/run-tests.el" \
    --eval "(progn (setq debug-on-error t) (ecc-load-tests) (ert-run-tests-batch-and-exit \".*\"))"

# Cleanup
rm -f "$MOCK_FILE"

# EOF