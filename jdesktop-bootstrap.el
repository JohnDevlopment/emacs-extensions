;; *- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

(use-package jdesktop
  :load-path "~/github/desktop-plus/"
  :demand t
  :config
  (jdesktop-default-bindings))

(provide 'jdesktop-bootstrap)
;;; jdesktop-bootstrap.el ends here
