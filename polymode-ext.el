;; -*- lexical-binding: t; -*-

(require 'polymode)

;; ### Host modes

(define-hostmode poly-org-hostmode
  :mode #'org-mode
  :protect-font-lock t)



;; ### Inner modes

(define-innermode poly-generic-eval-innermode
  nil
  "Innermode for file variables."
  :head-matcher "^\\s<+ eval: "
  :tail-matcher "$"
  :mode #'emacs-lisp-mode
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

(define-polymode poly-org-mode
  nil
  "A variation of `org-mode' for Poly mode."
  :hostmode 'poly-org-hostmode
  :innermodes '(poly-org-eval-innermode))

;; ### Advice

(advice-add 'hack-local-variables :around #'polymode-inhibit-in-indirect-buffers)

(provide 'polymode-ext)
;;; polymode-ext.el ends here
