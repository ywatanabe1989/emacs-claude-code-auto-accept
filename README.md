<!-- ---
!-- Timestamp: 2025-05-07 11:53:01
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->

# Emacs Claude Code

![Demo GIF](./docs/emacs-gif-screenshot-2025-05-05-12:04:36.gif)

This package provides integration with Claude Code in Emacs using Vterm.

## Features
- **Multi-buffer support**:
  - Work with multiple Claude instances simultaneously
  - Register, switch between, and manage multiple Claude buffers
  - Visual indicators show which buffer is active
- **Auto-accept mode**: 
  - Automatically respond "yes" to confirmation prompts
  - Automatically send "continue" when Claude is waiting for input
  - Uses both hook-based and timer-based approaches for reliable detection
- **Quick send functions**: 
  - Send regions, buffers, or custom prompts to Claude
  - Run Claude Code directly from Emacs
- **Repository utilities**:
  - Copy and format entire repositories for Claude to analyze
  - Intelligent file filtering and formatting

## Disclaimer
**AUTOMATIC ACCEPTANCE IS DANGEROUS. WE DO NOT ASSUME ANY RESPONSIBILITY.**

## Installation
```elisp
(require 'ecc)
```

## Usage

### Multi-buffer Support
- `M-x ecc-buffer-new`: Create a new Claude buffer and register it
- `M-x ecc-buffer-next`: Switch to the next registered Claude buffer
- `M-x --ecc-buffer-create-list`: List all registered Claude buffers
- `M-x ecc-buffer-register-active-buffer`: Switch to a specific Claude buffer
- `M-x ecc-buffer-unregister-buffer`: Remove a buffer from the Claude buffer list

### Auto-accept Mode
To start auto-accepting in a vterm buffer:
1. Run Claude Code in a vterm buffer
2. Make the buffer active with `M-x ecc-auto-enable`
   - This will register the current buffer as a Claude buffer
   - The buffer will be highlighted in the mode line

To stop auto-accepting:
`M-x ecc-auto-disable`

### Sending Content to Claude
- `M-x ecc-run-on-region`: Send selected region to Claude
- `M-x ecc-run-on-buffer`: Send current buffer to Claude
- `M-x ecc-run-quick`: Quick prompt from minibuffer

### Repository Utilities
- `M-x ecc-repository-copy-contents`: Format repository content for Claude

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->