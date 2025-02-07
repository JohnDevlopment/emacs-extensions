# Emacs Extensions

## Customize

### `package-archives`

| Archive name | URL                            |
|--------------|--------------------------------|
| melpa        | https://melpa.org/packages/    |
| gnu          | https://elpa.gnu.org/packages/ |

## Dependencies

- `desktop+` (melpa)
- `codeium` (melpa; optional)

## Default Installation

Move all lisp files into `~/.emacs.d/extensions` and put this into your init file:

```lisp
(load "~/.emacs.d/extensions/extensions.el")
```

Copy all the lisp files and `packages/` into the destination directory.

## Extensions

<!--
#+ORGTBL: SEND test orgtbl-to-markdown
| Extension | File           | External Packages | Autoload |
|-----------+----------------+-------------------+----------|
|           | <15>           | <30>              | &#9746;  |
| Buffers   | buffers-ext.el |                   | &#9746;  |
| Codeium   | codeium-ext.el | codeium           | &#9746;  |
| C         | c-ext.el       |                   | &#9745;  |
| Imenu     | imenu-ext.el   |                   | &#9746;  |
| Keymaps   | keymaps-ext.el | move-text         | &#9746;  |
| LSP       | lsp-ext.el     | lsp-mode          | &#9746;  |
| Rust      | rust-ext.el    | rust-mode, rustic | &#9745;  |
| HTML      | html-ext.el    |                   | &#9745;  |
|           |                |                   | &#9746;  |
|           |                |                   | &#9746;  |
-->

<!-- BEGIN RECEIVE ORGTBL test -->
| Extension | File | External Packages | Autoload |
|--:|--:|--:|--:|
|  | <15> | <30> | &#9746; |
| Buffers | buffers-ext.el |  | &#9746; |
| Codeium | codeium-ext.el | codeium | &#9746; |
| C | c-ext.el |  | &#9745; |
| Imenu | imenu-ext.el |  | &#9746; |
| Keymaps | keymaps-ext.el | move-text | &#9746; |
| LSP | lsp-ext.el | lsp-mode | &#9746; |
| Rust | rust-ext.el | rust-mode, rustic | &#9745; |
| HTML | html-ext.el |  | &#9745; |
|  |  |  | &#9746; |
|  |  |  | &#9746; |
<!-- END RECEIVE ORGTBL test -->
