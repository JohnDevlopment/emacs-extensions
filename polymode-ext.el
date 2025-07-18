;; -*- lexical-binding: t; -*-

(require 'polymode)

;; ### Host modes

(define-hostmode poly-markdown-hostmode
  :mode #'markdown-mode)

(define-hostmode poly-org-hostmode
  :mode #'org-mode
  :protect-font-lock t)

(define-hostmode poly-plantuml-hostmode
  :mode #'plantuml-mode)

(define-hostmode poly-shell-script-hostmode
  :mode #'shell-script-mode)

;; ### Inner modes

(define-innermode poly-generic-eval-innermode
  nil
  "Innermode for file variables."
  :head-matcher "^\\s<+ eval: "
  :tail-matcher "$"
  :mode #'emacs-lisp-mode
  :allow-nested nil)

;; --- HTML

(define-innermode poly-html-css-innermode
  nil
  "Innermode for CSS <style> tags in HTML."
  :head-matcher "<style>\\s-*"
  :head-mode 'host
  :tail-matcher "\\s-*</style>"
  :tail-mode 'host
  :mode #'css-mode
  :allow-nested nil)

;; --- Markdown

(define-auto-innermode poly-markdown-fenced-code-innermode
  nil
  "Mode for fenced code blocks."
  :head-matcher (cons "^[ \t]*\\(```[[:alpha:]].*$\\)" 1)
  :tail-matcher (cons "^[ \t]*\\(```\\)[ \t]*$" 1)
  :mode-matcher (cons "```[ \t]*\\([[:alpha:]][^ \t\n;=,]\\)" 1)
  :head-mode 'host
  :tail-mode 'host)

;; --- PlantUML

(define-innermode poly-plantuml-wbs-innermode nil
  "Innermode for WBS."
  :head-matcher "^@startwbs$"
  :head-mode 'host
  :tail-matcher "^@endwbs$"
  :tail-mode 'host
  :mode #'outline-mode
  :allow-nested nil)

(define-innermode poly-plantuml-json-innermode nil
  "Innermode for JSON."
  :head-matcher (cons (rx (group bol "@startjson\n" (*? anything))
			  bol "{" eol)
		      1)
  :head-mode 'host
  :tail-matcher "^@endjson$"
  :tail-mode 'host
  :mode #'json-mode
  :allow-nested nil)

;; --- Org

(define-innermode poly-org-eval-innermode
  nil
  "Innermode for file variables."
  :head-matcher "^# eval: "
  :tail-matcher "$"
  :mode #'emacs-lisp-mode
  :allow-nested nil)

(define-innermode poly-org-macro-eval-innermode
  nil
  "Innermode for Emacs Lisp code in macro replacements."
  :head-matcher "^#\\+macro: .*?(eval"
  :head-mode 'host
  :tail-matcher ")\n"
  :tail-mode 'host
  :mode #'emacs-lisp-mode
  :allow-nested nil)

(define-auto-innermode poly-org-inline-source-innermode
  :head-matcher "\\bsrc_[a-z][a-z0-9_-]*\\(?:\\[.*?\\]\\)?{"
  :head-mode 'host
  :tail-matcher (cons "{.*}\\($\\)" 1)
  :tail-mode 'host
  :mode-matcher (cons "src_\\([a-z][a-z0-9_-]*\\)" 1))

(define-innermode poly-org-html-innermode
  nil
  "Innermode for HTML snippets."
  :head-matcher "^#\\+\\(?:HTML_HEAD\\|html_head\\):[ \t]+"
  :head-mode 'host
  :tail-matcher "$"
  :tail-mode 'host
  :mode #'html-mode
  :allow-nested nil)

;; --- Shell Script

(define-innermode poly-shell-script-here-doc-innermode nil
  "Here documents."
  :head-matcher (cons "<<-?[ \t]*\\(EOF\\)$" 1)
  :tail-matcher "^EOF$"
  :tail-mode 'host
  :head-mode 'host
  :mode #'shell-script-mode
  :allow-nested nil)

;; ### Poly modes

(define-polymode poly-emacs-lisp-file-variables-mode
  nil
  "A variation of `emacs-lisp-mode' for Polymode."
  :hostmode 'poly-emacs-lisp-hostmode
  :innermodes '(poly-generic-eval-innermode))

(define-polymode poly-html-mode
  poly-html-root-polymode
  "A variation of `html-mode' for Poly mode."
  :hostmode 'poly-html-hostmode
  :innermodes '(poly-html-css-innermode))

(define-polymode poly-markdown-mode
  :hostmode 'poly-markdown-hostmode
  :innermodes '(poly-markdown-fenced-code-innermode))

(define-polymode poly-org-mode
  nil
  "A variation of `org-mode' for Poly mode."
  :hostmode 'poly-org-hostmode
  :innermodes '(poly-org-eval-innermode
		poly-org-inline-source-innermode
		poly-org-html-innermode
		poly-org-macro-eval-innermode))

(define-polymode poly-plantuml-mode nil
  "PlantUML poly mode."
  :hostmode 'poly-plantuml-hostmode
  :innermodes '(poly-plantuml-json-innermode
		poly-plantuml-wbs-innermode))

(define-polymode poly-shell-script-mode
  nil
  "A variation of `shell-script-mode' for Poly mode."
  :hostmode 'poly-shell-script-hostmode
  :innermodes '(poly-shell-script-here-doc-innermode))

;; ### Advice

(advice-add 'hack-local-variables :around #'polymode-inhibit-in-indirect-buffers)

(provide 'polymode-ext)
;;; polymode-ext.el ends here
