;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")
(extension-check-requires use-package)

(require 'use-package)

(use-package jdesktop
  :load-path "~/github/desktop-plus/"
  :demand t
  :config
  (jdesktop-default-bindings))

(extension-provide 'jdesktop-bootstrap)
;;; jdesktop-bootstrap.el ends here
