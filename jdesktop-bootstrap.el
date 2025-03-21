;; *- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

(use-package jdesktop
  :load-path "~/github/desktop-plus/"
  :functions
  jdesktop-default-bindings
  :config
  (jdesktop-default-bindings))

(provide 'jdesktop-bootstrap)
;;; jdesktop-bootstrap.el ends here
