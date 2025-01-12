;;; outline-ext.el --- Outline mode extension.       -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(eval-when-compile
  (require 'outline))

(define-key outline-mode-map (kbd "<S-return>") #'outline-insert-heading)

;;;###autoload
(defun outline--extra-hook ()
  t)

;;;###autoload
(add-hook 'outline-mode-hook #'outline--extra-hook)

(provide 'outline-ext)

;;; outline-ext.el ends here
