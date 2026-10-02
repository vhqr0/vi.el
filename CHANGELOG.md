# Changelog

## 0.2.0

Added:

- Per major mode override maps: `vim-define-major-mode-map` and
  `vim-major-mode-map-set`.  A major mode uses the maps of its nearest
  ancestor, and the maps of a major mode inherit those of its parent.
- `g-` narrow op and `gy` eval op, with `vim-eval-function-alist`.
- `g;` to go to the last change.
- `m` in visual state jumps to the matching item.
- `j` and `k` in op maps, so `dj` and `yk` work when a major mode rebinds
  `j` and `k`.

Changed:

- Vim maps are in `emulation-mode-map-alists`, taking precedence over minor
  mode maps.
- `vim-normal-mode`, `vim-visual-mode` and `vim-insert-mode` are buffer
  local variables instead of minor modes.
- `C-z` is no longer bound to escape.
- `R` ends on `RET` or `C-j`, and no longer on `C-z`.

## 0.1.0

Initial release.

- Normal, visual and insert states managed by `vim-global-mode`.
- Moves, text objects, ops and cmds with counts and `.` repeat.
- Find (`f F t T ; ,`) and isearch based search (`/ ? n N`).
- Surround (`ds cs ys`).
