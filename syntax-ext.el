(defun my/mcpyrate-syntax-highlight-setup ()
  "Set up additional syntax highlighting for `mcpyrate` in Python mode."
  ;; adapted from code in dash.el
  (let ((more-keywords '("macros" "dialects"
                         "q" "u" "n" "a" "s" "t" "h"))
        ;; How to make Emacs recognize your magic variables. Only for the anaphoric if demo.
        ;; A list, like `more-keywords`, even though in the example there is only one item.
        (magic-variables '("it")))
    (font-lock-add-keywords 'python-mode `((,(concat "\\_<" (regexp-opt magic-variables 'paren) "\\_>")
                                            1 font-lock-variable-name-face)) 'append)
    (font-lock-add-keywords 'python-mode `((,(concat "\\_<" (regexp-opt more-keywords 'paren) "\\_>")
                                            1 font-lock-keyword-face)) 'append)
    ))

(add-hook 'python-mode-hook #'my/mcpyrate-syntax-highlight-setup)
