;;; outline-ext.el --- Outline mode extension.       -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(eval-when-compile
  (require 'outline))

(define-key outline-mode-map (kbd "<S-return>") #'outline-insert-heading)

(fext-defadvice outline-demote (after outline-demote)
  (goto-char (line-end-position)))

(fext-defadvice outline-premote (after outline-premote)
  (goto-char (line-end-position)))

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
