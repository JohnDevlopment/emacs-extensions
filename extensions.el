(add-to-list 'load-path "~/.emacs.d/extensions/packages")

(eval-when-compile
  (load-extension "custom-ext"))

(load-extension "codeium-ext")
(load-extension "custom-ext")

;; General functions and key bindings
(load-extension "general")
(load-extension "keymaps-ext")

;; lsp activation
(load-extension "lsp-ext")

;; Buffer-related functions
(load-extension "buffers-ext")

;; imenu
(load-extension "imenu-ext")

;; syntax highlighting
(load-extension "syntax-ext")

;; mode extensions
(load-extension "modes")
