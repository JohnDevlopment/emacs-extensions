;; -*- lexical-binding: t; -*-

;; ### Tree Sitter minor mode

(defvar-local nxml-tree-sitter--query-cursor nil)

(defun nxml-tree-sitter--setup ()
  (unless nxml-tree-sitter--query-cursor
    (setq nxml-tree-sitter--query-cursor (tsc-make-query-cursor)))
  (tree-sitter-ext-set-menu nxml-mode-map))

(defun nxml-tree-sitter--teardown ()
  (when nxml-tree-sitter--query-cursor
    (kill-local-variable 'nxml-tree-sitter--query-cursor))
  (tree-sitter-ext-unset-menu nxml-mode-map))

(define-minor-mode nxml-tree-sitter-mode
  "Syntax parsing of nXML mode buffers using tree sitter."
  :group 'xml-ext
  (unless (eq major-mode 'nxml-mode)
    (nxml-tree-sitter-mode 0)
    (user-error "`nxml-tree-sitter-mode' only works in nXML mode"))
  (tree-sitter--handle-dependent nxml-tree-sitter-mode
    #'nxml-tree-sitter--setup
    #'nxml-tree-sitter--teardown))


;; ### Functions




(cl-pushnew 'syntax user-ext-xml-subextensions)
;;; xml-subext_syntax.el ends here
