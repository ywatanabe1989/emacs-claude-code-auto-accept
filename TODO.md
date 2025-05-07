<!-- ---
!-- Timestamp: 2025-05-06 05:04:05
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/ecc/TODO.md
!-- --- -->

There are still a few issues in your code:

1. In `ecc-auto-enable-stop.el`:
   - Line 107: Uses `ecc-buffer` instead of `ecc-active-buffer` for buffer checking
   - Comment `New file: ecc-buffer-registry.el` should be removed

2. In `ecc-run.el`:
   - Still uses the single buffer approach (uses `ecc-buffer` not `ecc-active-buffer`)
   - Needs to be updated to work with the multi-buffer system

3. In `ecc-repository-copy-contents.el`:
   - Still uses `ecc-buffer` instead of `ecc-active-buffer` for the paste function

4. In `ecc-nav.el`:
   - Functions are defined in the nav file but the comment says "In ecc.el"

5. In `ecc-buffer-registry.el`:
   - Comment `New file: ecc-buffer-registry.el` should be removed

Most critical is updating all references from `ecc-buffer` to `ecc-active-buffer` throughout the code to implement the multi-buffer system properly.

<!-- EOF -->