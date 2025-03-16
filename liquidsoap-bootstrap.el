;; -*- lexical-binding: t; -*-

(use-package liquidsoap-mode
  :load-path "~/.opam/soap/share/emacs/site-lisp"
  :if (locate-library "liquidsoap-mode")
  :mode "\\.liq\\'"
  :defer t
  :commands
  liquidsoap-mode)

(provide 'liquidsoap-bootstrap)
;;; liquidsoap-bootstrap.el ends here
