;; *- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (declare-function jdesktop-default-bindings "jdesktop"))

(eval-and-compile
  (defcustom user-ext-desktop-load-path ""
    "Load path for `use-package' of package `jdesktop'."
    :type 'file
    :group 'desktop-ext))

(use-package jdesktop
  :if (locate-library "jdesktop")
  :defer t
  :config
  (jdesktop-default-bindings))

(provide 'jdesktop-bootstrap)
;;; jdesktop-bootstrap.el ends here
