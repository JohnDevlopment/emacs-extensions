;; -*- lexical-binding: t; -*-

(use-package jinja2-mode
  :load-path "~/github/jinja2-mode/"
  :mode ("\\.jinja\\'" . jinja2-mode)
  :mode ("\\.j2\\'" . jinja2-mode)
  :commands
  jinja2-mode)

(provide 'jinja2-bootstrap)
;;; jinja2-bootstrap.el ends here
