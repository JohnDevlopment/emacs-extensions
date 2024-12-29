(require 'cl-lib)

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

(defun make-ppss-easy (&optional ppss)
  "Constructor of type `ppss' from PPSS.
If PPSS is omitted, then it is set to the value of
`syntax-ppss'."
  (declare (pure t))
  (let ((ppss (or ppss (syntax-ppss))))
    (make-ppss :depth                    (nth 0 ppss)
	       :innermost-start          (nth 1 ppss)
	       :last-complete-sexp-start (nth 2 ppss)
	       :string-terminator        (nth 3 ppss)
	       :comment-depth            (nth 4 ppss)
	       :quoted-p                 (nth 5 ppss)
	       :min-depth                (nth 6 ppss)
	       :comment-style            (nth 7 ppss)
	       :comment-or-string-start  (nth 8 ppss)
	       :open-parens              (nth 9 ppss)
	       :two-character-syntax     (nth 10 ppss))))
