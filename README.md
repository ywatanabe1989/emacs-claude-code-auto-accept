<!-- ---
!-- Timestamp: 2025-05-06 02:15:22
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->

# Emacs Claude Code

![Demo GIF](./docs/emacs-gif-screenshot-2025-05-05-12:04:36.gif)

This package provides integration with Claude Code in Emacs using Vterm.

## Features
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
(require 'emacs-claude-code)
```

## Usage

### Auto-accept Mode
To start auto-accepting in a vterm buffer:
1. Run Claude Code in a vterm buffer
2. Rename the buffer as "*CLAUDE-CODE*" (= `emacs-claude-buffer-name`)
3. In the "*CLAUDE-CODE*" buffer, `M-x emacs-claude-code-start`

To stop auto-accepting:
`M-x emacs-claude-code-stop`

### Sending Content to Claude
- `M-x emacs-claude-run-on-region`: Send selected region to Claude
- `M-x emacs-claude-run-on-buffer`: Send current buffer to Claude
- `M-x emacs-claude-run-quick`: Quick prompt from minibuffer

### Repository Utilities
- `M-x emacs-claude-copy-repository`: Format repository content for Claude

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->