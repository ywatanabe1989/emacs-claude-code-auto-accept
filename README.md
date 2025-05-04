<!-- ---
!-- Timestamp: 2025-05-04 22:50:34
!-- Author: root
!-- File: /root/.emacs.d/lisp/claude/README.md
!-- --- -->

<!-- ---
!-- Timestamp: 2025-05-04 22:10:43
!-- Author: root
!-- File: /root/.emacs.d/lisp/claude/README.md
!-- --- -->
# Emacs Claude Auto Accept
This package enables automatic acceptance for Claude Code on Vterm.
- Automatically respond "yes" to y/n confirmation prompts
- Automatically send "continue" when Claude is waiting for input
- Uses both hook-based and timer-based approaches for reliable detection

## Disclaimer
AUTOMATIC ACCEPTANCE IS DANGEROUS. WE DO NOT ASSUME ANY RESPONSIBILITY.

## Installation
```elisp
(require 'emacs-claude-send)
```

## Configuration
The main variables you can customize:
- `emacs-claude-buffer-name`: The name of the buffer where Claude is running (default: "*CLAUDE-CODE*")
- `emacs-claude-prompt-y/n`: Text pattern to detect Y/N prompts
- `emacs-claude-prompt-waiting`: Text pattern to detect waiting prompts

## Usage
To start auto-accepting in a vterm buffer:
```
M-x emacs-claude-auto-accept-start
```

To stop auto-accepting:
```
M-x emacs-claude-auto-accept-stop
```

The package uses both a timer (checking every second) and vterm-update-functions for reliable prompt detection.

You can also manually trigger the functions:
- `M-x emacs-claude-auto-accept` - Check for and respond to all prompt types
- `M-x --emacs-claude-auto-send-y` - Respond only to Y/N prompts
- `M-x --emacs-claude-auto-send-continue` - Respond only to "continue" prompts

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->