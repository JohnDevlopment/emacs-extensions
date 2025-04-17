;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'outline))

(define-key outline-mode-map (kbd "<S-return>") #'outline-insert-heading)

(defun outline-ext--after-premote-demote (&rest _args)
  (goto-char (line-end-position)))

(advice-add 'outline-promote :after #'outline-ext--after-premote-demote)
(advice-add 'outline-demote :after #'outline-ext--after-premote-demote)

;;;###autoload
(defun outline--extra-hook () t)

;;;###autoload
(defun outline-minor-mode--extra-hook () t)

;;;###autoload
(add-hook 'outline-minor-mode-hook #'outline-minor-mode--extra-hook)

;;;###autoload
(add-hook 'outline-mode-hook #'outline--extra-hook)

(provide 'outline-ext)

;;; outline-ext.el ends here
