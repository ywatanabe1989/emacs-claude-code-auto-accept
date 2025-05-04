<!-- ---
!-- Timestamp: 2025-05-05 00:44:38
!-- Author: ywatanabe
!-- File: /ssh:sp:/home/ywatanabe/proj/llemacs/home/.emacs.d/lisp/emacs-claude-code-auto-accept/README.md
!-- --- -->

# Emacs Claude Auto Accept

This package enables automatic acceptance for Claude Code on Vterm.
- Automatically respond "yes" to y/n confirmation prompts
- Automatically send "continue" when Claude is waiting for input
- Uses both hook-based and timer-based approaches for reliable detection

## Disclaimer
**AUTOMATIC ACCEPTANCE IS DANGEROUS. WE DO NOT ASSUME ANY RESPONSIBILITY.**

## Installation
```elisp
(require 'emacs-claude-auto-accept)
```

## Usage
To start auto-accepting in a vterm buffer:
1. Run claude code on vterm buffer
2. Rename the buffer as "*CLAUDE-CODE*" (= `emacs-claude-buffer-name`)
3. In the "*CLAUDE-CODE*" buffer, `M-x emacs-claude-auto-accept-start`

To stop auto-accepting:
`M-x emacs-claude-auto-accept-stop`

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->