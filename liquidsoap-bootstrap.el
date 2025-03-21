;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-ext))

(use-package liquidsoap-mode
  :load-path "~/.opam/soap/share/emacs/site-lisp"
  :mode ("\\.liq\\'" . liquidsoap-mode)
  :defer t
  :commands
  liquidsoap-mode)

(provide 'liquidsoap-bootstrap)
;;; liquidsoap-bootstrap.el ends here
