;; Dired
(load-extension "dired-ext" t)

;; Org
(load-extension "org-ext" t)

;; Markdown
(load-extension "markdown-ext")

;; DText
(load-extension "dtext-ext" t)

;; Lua
(load-extension "lua-ext")

;; Rust
(load-extension "rust-ext")

;; C/C++
(load-extension "c-ext")

;; sh
(load-extension "sh-ext")

;; yasnippets
(load-extension "yasnippets-ext")

;; Python
(load-extension "python-ext")
(load-extension "python-sphinx-ext")

;; Javscript
(add-hook 'js-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq tab-width 2)))

;; BB-code
(load-extension "bbcode-ext")

;; Latex
(load-extension "latex-ext")

;; Elisp
(load-extension "elisp-ext")

;; Tcl
(load-extension "tcl-ext")

;; Origami
(load-extension "origami-ext")

;; Help

(defun help--extra-hook ()
  (define-key help-mode-map (kbd "C-c q") #'kill-and-quit))

;; Liquidsoap
(load-extension "liquidsoap-bootstrap")
(load-extension "liquidsoap-ext")

;; ====
