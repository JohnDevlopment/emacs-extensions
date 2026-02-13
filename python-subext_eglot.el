;; -*- lexical-binding: t; -*-

(extension-check-requires eglot)

(setf (alist-get '(python-mode python-ts-mode)
		 eglot-server-programs
		 nil nil #'equal)
      (eglot-alternatives
       '(("rass" "basedruff")
	 ("basedpyright-langserver" "--stdio")
	 ("ruff" "server") "ruff-lsp")))

(cl-pushnew 'eglot user-ext-python-subextensions)
