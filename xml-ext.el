;; -*- lexical-binding: t; -*-

(require 'nxml-mode)
(require 'tree-sitter)


;; ### Customization

(defgroup xml-ext nil
  "XML extension."
  :group 'user-extensions)

(defcustom user-ext-xml-mode-hook nil
  "Hook run by `xml-ext'."
  :type 'hook
  :group 'xml-ext
  :options '(nxml-tree-sitter-mode))


;; ### Variables

(defvar user-ext-xml-subextensions nil "List of loaded subextensions.")


;; ### Functions

(load-extension-safe "xml-subext_syntax")


;; ### Hook

;;;###autoload
(defun nxml--extra-hook () t)

;;;###autoload
(add-hook 'nxml-mode-hook #'nxml--extra-hook)


(extension-provide 'xml-ext user-ext-xml-subextensions)
;;; xml-ext.el ends here
