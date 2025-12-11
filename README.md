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

### Autoloaded Extensions

<!--
#+ORGTBL: SEND extensions orgtbl-to-markdown
| Extension | File            | External Packages               |
|-----------+-----------------+---------------------------------|
| Basic     | basic-ext.el    | basic-mode                      |
| C         | c-ext.el        |                                 |
| HTML      | html-ext.el     |                                 |
| Markdown  | markdown-ext.el | markdown-toc                    |
| Rust      | rust-ext.el     | company-capf, rust-mode, rustic |
-->

<!-- BEGIN RECEIVE ORGTBL extensions -->
| Extension | File | External Packages |
|--:|--:|--:|
| Basic | basic-ext.el | basic-mode |
| C | c-ext.el |  |
| HTML | html-ext.el |  |
| Markdown | markdown-ext.el | markdown-toc |
| Rust | rust-ext.el | company-capf, rust-mode, rustic |
<!-- END RECEIVE ORGTBL extensions -->

### Other Extensions

<!--
#+ORGTBL: SEND other orgtbl-to-markdown
| Extension | File           | External Packages |
|-----------+----------------+-------------------|
| Buffers   | buffers-ext.el |                   |
| Codeium   | codeium-ext.el | codeium           |
| Imenu     | imenu-ext.el   |                   |
| Keymaps   | keymaps-ext.el | move-text         |
-->

<!-- BEGIN RECEIVE ORGTBL other -->
| Extension | File | External Packages |
|--:|--:|--:|
| Buffers | buffers-ext.el |  |
| Codeium | codeium-ext.el | codeium |
| Imenu | imenu-ext.el |  |
| Keymaps | keymaps-ext.el | move-text |
<!-- END RECEIVE ORGTBL other -->
