;;; lua-ext --- Lua mode extension.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lua-mode)

;;;###autoload
(defun lua-add-require (modname)
  "Inserts a require statement at point. MODNAME is the name of a Lua module."
  (interactive "sModule: ")
  (when (not (string= modname ""))
    (insert (format "local %s = require '%s'" modname modname))))

;;;###autoload
(defun lua--extra-mode-hook ()
  (define-key lua-mode-map (kbd "C-c C-t r") #'lua-add-require))

;;;###autoload
(add-hook 'lua-mode-hook #'lua--extra-mode-hook)

(provide 'lua-ext)

;;; lua-ext.el ends here
