;;; rst-ext.el --- ReST mode extension.              -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(define-key rst-mode-map (kbd "C-c C-l <tab>") #'rst-insert-list)

;;;###autoload
(defun rst-mode--extra-hook ()
  "Extra hook for `rst-mode'."
  (enable-wrap))

;;;###autoload
(add-hook 'rst-mode-hook #'rst-mode--extra-hook)

(provide 'rst-ext)
;;; rst-ext.el ends here
