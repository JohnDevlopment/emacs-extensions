# Emacs Extensions

## Customize

### `package-archives`

| Archive name | URL                            |
|--------------|--------------------------------|
| melpa        | https://melpa.org/packages/    |
| gnu          | https://elpa.gnu.org/packages/ |

## Dependencies

- `desktop+` (melpa)

## Default Installation

Move `extensions.el` into `~/.emacs.d/extensions` and put this into your init file:

```lisp
(load "~/.emacs.d/extensions/extensions.el")
```

Copy all the lisp files and `packages/` into the destination directory.
