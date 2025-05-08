<!-- ---
!-- Timestamp: 2025-05-08 02:17:22
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/README.md
!-- --- -->


# Emacs Claude Code

[![Build Status](https://github.com/ywatanabe1989/emacs-claude-code/workflows/tests/badge.svg)](https://github.com/ywatanabe1989/emacs-claude-code/actions)

A streamlined Emacs interface for Claude AI coding assistance.

## Overview

Emacs Claude Code (ECC) enhances your workflow by seamlessly integrating Claude's AI capabilities directly into Emacs. The package provides:

- Simple buffer management for Claude interactions
- Automatic response handling
- Template system for common queries
- Region/buffer processing
- Repository analysis capabilities

## Installation

### Requirements

- Emacs 28+
- vterm package
- Claude account (Anthropic)

### Setup

1. Clone this repository:
   ```
   git clone https://github.com/ywatanabe/emacs-claude-code.git ~/.emacs.d/lisp/emacs-claude-code
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/lisp/emacs-claude-code")
   (require 'emacs-claude-code)
   ```

## Usage
All commands are available under the `C-c c` prefix:

### Buffer Management
- `C-c c c` - Create new Claude session
- `C-c c n` - Navigate to next Claude buffer
- `C-c c p` - Navigate to previous Claude buffer
- `C-c c l` - List all Claude buffers

### Interaction
- `C-c c a` - Toggle auto-accept mode
- `C-c c y` - Manually accept Claude prompt
- `C-c c i` - Initialize/restart Claude session
- `C-c c ESC` - Interrupt Claude processing

### Content Processing
- `C-c c r` - Run Claude on region
- `C-c c b` - Run Claude on buffer
- `C-c c q` - Quick query to Claude
- `C-c c t` - Send a template

### Templates
- `C-c c T c` - Create new template
- `C-c c T e` - Edit existing template

### Memory Management
- `C-c c m` - Edit Claude memory
- `C-c c h c` - Compact history
- `C-c c h l` - Clear history

### Repository
- `C-c c R` - Copy repository contents

## Auto-Accept Mode

ECC provides an Auto-Accept mode that automatically handles Claude's interactive prompts without requiring manual intervention. This is particularly useful for long-running interactions where Claude frequently asks for confirmation or choices.

### How to Use

1. **Toggle Auto-Accept Mode**: Use `C-c c a` to toggle auto-accept mode on/off

2. **Automatic Actions**:
   - When Claude displays a Y/N prompt, auto-accept automatically selects "Y"
   - For multi-option prompts (Y/Y/N), it selects the middle option
   - When Claude waits for confirmation to continue, it automatically sends the continue command

3. **Notifications**:
   - Desktop notifications are sent when auto-accept is enabled/disabled
   - Notifications are also sent each time Claude prompts are automatically handled

### Configuration

Auto-accept mode can be configured through these variables:

```elisp
;; Set auto-accept interval (defaults to 1 second)
(setq ecc-auto-interval-sec 2)

;; Enable auto-accept when ECC starts
(setq ecc-auto-enable t)
```

When auto-accept mode is active, you'll see "Auto" in the mode line indicator.

## Customization

Configure through the `customize` interface:
```elisp
M-x customize-group RET emacs-claude-code RET
```

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->