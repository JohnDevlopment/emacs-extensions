(eval-when-compile
  (require 'lua-mode))

(defun lua-add-require (modname)
  "Inserts a require statement at point. MODNAME is the name of a Lua module."
  (interactive "sModule: ")
  (when (not (string= modname ""))
    (insert (format "local %s = require '%s'" modname modname))))

(defun lua--extra-mode-hook ()
  (define-key lua-mode-map (kbd "C-c C-t r") #'lua-add-require))

(add-hook 'lua-mode-hook #'lua--extra-mode-hook)
