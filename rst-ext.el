;; -*- lexical-binding: t; -*-

(keymaps-ext-set-keymap rst-mode-map "C-c C-l <tab>" #'rst-insert-list)

;;;###autoload
(defun rst-mode--extra-hook ()
  "Extra hook for `rst-mode'."
  (enable-wrap))

;;;###autoload
(add-hook 'rst-mode-hook #'rst-mode--extra-hook)

(extension-provide 'rst-ext)
;;; rst-ext.el ends here
