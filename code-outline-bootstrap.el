;; -*- lexical-binding: t; -*-

(eval-and-compile
  (defcustom user-ext-code-outline-load-path ""
    "Load path for `code-outline'."
    :type 'directory
    :group 'user-extensions))

(use-package code-outline
  :load-path user-ext-code-outline-load-path
  :defer t)

(provide 'code-outline-bootstrap)
;;; code-outline-bootstrap.el ends here
