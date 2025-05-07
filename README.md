<!-- ---
!-- Timestamp: 2025-05-07 13:24:27
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->

# Emacs Claude Code

![Demo GIF](./docs/emacs-gif-screenshot-2025-05-05-12:04:36.gif)

This package provides integration with Claude Code in Emacs using Vterm.

## Features
- **Multi-buffer support**:
  - Multiple Claude instances supported
  - Navigation Claude buffers
  - Visual indicators available

- **Auto-accept mode**: 
  **AUTOMATIC ACCEPTANCE IS DANGEROUS. WE DO NOT ASSUME ANY RESPONSIBILITY.**
  - Automatically respond "yes" to confirmation prompts
  - Automatically send "continue" when Claude is waiting for input
  - Uses both hook-based and timer-based approaches for reliable detection

- **Quick send functions**: 
  - Send regions, buffers, or custom prompts to Claude
  - Run Claude Code directly from Emacs
  - Clear and restart Claude sessions
  - Interrupt running Claude operations

- **Repository utilities**:
  - Copy and format entire repositories for Claude to analyze
  - Intelligent file filtering and formatting

- **Template system**:
  - Create and manage prompt templates
  - Easily reuse common prompt patterns
  - Fast access to frequently used instructions

## Installation
```elisp
(require 'emacs-claude-code)
```

## Usage

### Multi-buffer Support
- `M-x ecc-buffer-create`: Create a new Claude buffer and register it
- `M-x ecc-buffer-next`: Switch to the next registered Claude buffer
- `M-x ecc-buffer-prev`: Switch to the previous registered Claude buffer
- `M-x ecc-buffer-list-buffers`: List all registered Claude buffers
- `M-x ecc-buffer-switch-to-active-buffer`: Switch to a specific Claude buffer
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
- `M-x ecc-send-init`: Clear current conversation and restart Claude
- `M-x ecc-send-interrupt`: Interrupt any running Claude operation
- `M-x ecc-send-template`: Send a custom response template
- `M-x ecc-send-clear-history`: Clear conversation history
- `M-x ecc-send-compact-history`: Compact conversation history to save tokens
- `M-x ecc-send-edit-memory`: Edit Claude's memory/previous messages

### Repository Utilities
- `M-x ecc-repository-copy-contents`: Format repository content for Claude

### Template System
- `M-x ecc-template-create`: Create a new prompt template
- `M-x ecc-template-edit`: Edit an existing template
- `M-x ecc-send-template`: Select and use a template

## Key Bindings
All commands are available through the keymap prefix `C-c c`.

| Key   | Function                       | Description                         |
|-------|--------------------------------|-------------------------------------|
| `c`   | `ecc-buffer-create`            | Create a new Claude buffer          |
| `n`   | `ecc-buffer-next`              | Switch to next Claude buffer        |
| `p`   | `ecc-buffer-prev`              | Switch to previous Claude buffer    |
| `l`   | `ecc-buffer-list-buffers`      | List all Claude buffers             |
| `a`   | `ecc-auto-toggle`              | Toggle auto-accept mode             |
| `y`   | `ecc-send-accept`              | Manually trigger accept action      |
| `r`   | `ecc-run-on-region`            | Run Claude on region                |
| `b`   | `ecc-run-on-buffer`            | Run Claude on buffer                |
| `q`   | `ecc-run-quick`                | Quick prompt from minibuffer        |
| `i`   | `ecc-send-init`                | Initialize/clear Claude session     |
| `ESC` | `ecc-send-interrupt`           | Interrupt running operation         |
| `t`   | `ecc-send-template`            | Send custom template response       |
| `h c` | `ecc-send-clear-history`       | Clear conversation history          |
| `h m` | `ecc-send-compact-history`     | Compact conversation history        |
| `h e` | `ecc-send-edit-memory`         | Edit Claude's memory                |
| `R`   | `ecc-repository-copy-contents` | Copy repository contents for Claude |
| `T c` | `ecc-template-create`          | Create a new template               |
| `T e` | `ecc-template-edit`            | Edit an existing template           |

## TODO
- [ ] Other terminal type support

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->