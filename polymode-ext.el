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


;; ### Inner modes

(define-innermode poly-generic-eval-innermode
  nil
  "Innermode for file variables."
  :head-matcher "^\\s<+ eval: "
  :tail-matcher "$"
  :mode #'emacs-lisp-mode
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


  nil
;; ### Poly modes

(define-polymode poly-emacs-lisp-file-variables-mode
  nil
  "A variation of `emacs-lisp-mode' for Polymode."
  :hostmode 'poly-emacs-lisp-hostmode
  :innermodes '(poly-generic-eval-innermode))

(define-polymode poly-markdown-mode
  :hostmode 'poly-markdown-hostmode
  :innermodes '(poly-markdown-fenced-code-innermode))

(define-polymode poly-org-mode
  nil
  "A variation of `org-mode' for Poly mode."
  :hostmode 'poly-org-hostmode
  :innermodes '(poly-org-eval-innermode))

(define-polymode poly-plantuml-mode nil
  "PlantUML poly mode."
  :hostmode 'poly-plantuml-hostmode
  :innermodes '(poly-plantuml-json-innermode
		poly-plantuml-wbs-innermode))

;; ### Advice

(advice-add 'hack-local-variables :around #'polymode-inhibit-in-indirect-buffers)

(provide 'polymode-ext)
;;; polymode-ext.el ends here
