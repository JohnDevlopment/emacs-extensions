;; -*- lexical-binding: t; -*-

(use-package liquidsoap-mode
  :load-path "~/.opam/soap/share/emacs/site-lisp"
  :mode ("\\.liq\\'" . liquidsoap-mode)
  :defer t
  :commands
  liquidsoap-mode)

(extension-provide 'liquidsoap-bootstrap)
;;; liquidsoap-bootstrap.el ends here
