;; -*- lexical-binding: t; -*-

(require 'json-mode)

(define-key json-mode-map (kbd "C-c ^") #'elisp-ext-jump-sexp-level)


;; ### Hooks

;;;###autoload
(defun json--extra-hook () t)

;;;###autoload
(add-hook 'json-mode-hook #'json--extra-hook)

(provide 'json-ext)
;;; json-ext.el ends here
