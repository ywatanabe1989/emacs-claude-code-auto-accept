<!-- ---
!-- Timestamp: 2025-05-08 13:06:48
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/CLAUDE.md
!-- --- -->

!!! IMPORTANT !!!
**DO NEVER EDIT THIS FILE. ONLY USER CAN REVISE THE CONTENTS.**

## Basic Rules and Prompts I often use; DO NOT UNDERSTAND THEM AS REQUESTS TO YOU AT THE CURENT MOMENT
- See `.claude/commands/general.md`
- See `.claude/commands/programming*.md`

## Project-Level Rules
## Core files and utility functions
- Main package: emacs-claude-code.el (requires all modules)
- Buffer management: ecc-buffer/ directory
- Template system: ecc-templates.el, templates/ directory
- State management: ecc-state.el, ecc-state-detect.el

## Code style guidelines
- Package prefix: `ecc-` for public, `--ecc-` for private functions/variables
- Use kebab-case: `ecc-buffer-name`, `ecc-prompt-pattern-y/n`
- Function naming: verb-noun pattern (ecc-buffer-verify-buffer)
- Begin files with lexical-binding: t declaration
- Include comprehensive docstrings for all functions

## Testing instructions
- All tests in tests/ directory with test- prefix
- ERT test framework with should/should-not assertions
- Each module should have corresponding test file
- Tests should clean up their own state

## Repository etiquette
- Keep commits focused on single changes
- Update tests with new features
- Follow existing module structure and naming

## Environment setup notes
- Read repo at https://github.com:ywatanabe1989/emacs-claude-code
- Say Hi, Yusuke! at startup

## Warnings and quirks
- Handle buffer validation carefully
- Use proper error handling with condition-case
- Buffer management system is critical - use existing conventions

## Puppeteer
https://github.com/modelcontextprotocol/servers/tree/c19925b8f0f2815ad72b08d2368f0007c86eb8e6/src/puppeteer
A Model Context Protocol server that provides browser automation capabilities using Puppeteer. This server enables LLMs to interact with web pages, take screenshots, and execute JavaScript in a real browser environment.


https://www.anthropic.com/engineering/claude-code-best-practices

<!-- EOF -->