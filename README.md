# vim.el

A small vim emulation built on Emacs commands.

Requires Emacs 31.1 or later.

## Install

```elisp
(add-to-list 'load-path "/path/to/vim.el")
(require 'vim)
(vim-global-mode 1)
```

Or with `package-vc-install`:

```elisp
(package-vc-install "https://github.com/vhqr0/vim.el")
```

## States

- normal: `vim-normal-mode`, the default in ordinary buffers.
- insert: `vim-insert-mode`, the default in minibuffers.
- visual: `vim-visual-mode`, entered automatically while the region is active.

`<escape>` returns to normal state.  Turning off
`vim-global-mode` leaves plain Emacs bindings.

## Keys

- Moves: `h j k l w e b ge W E B gE 0 ^ $ gg G % f F t T ; , / ? n N`.
- Text objects (after `i` / `a`): `w W f l p d h b B r a q Q`, plus
  `( { [ < ' "` as aliases.
- Ops: `d c y = gc gu gU g~ < > gJ`, and `ys` for surround.  Double the last
  key for the current line (`dd`, `gUU`).
- Cmds: `D C Y x X s S r ~ J i I a A o O p P ds cs`.
- Misc: `.` repeat, `u` undo, `U` redo, `v` / `V` select, `R` overwrite,
  `\` run the next key in Emacs bindings, `:` `M-x`.

A count goes either before the op or after it (`2dw`, `d2w`, `d2ib`) and may
be negative.

## Extending

New keys are defined with `vim-define-move`, `vim-define-tobj`,
`vim-define-op` and `vim-define-cmd`.  A move function takes a count; a
text object function marks the region like `mark-paragraph`; an op function
takes `beg` and `end`.

```elisp
(vim-define-op "gw" #'fill-region)
```

## Test

```sh
make test
```
