;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-ext))

(defvar user-ext-ebnf--init nil)

(ebnf-insert-style
 'custom 'default
 '(ebnf-syntax . (quote abnf)))

(--ignore
 (ebnf-delete-style 'custom))

;; ### Setup

;;;###autoload
(defun ebnf--extra-hook ()
  (cl-ext-unless user-ext-ebnf--init
      (modify-syntax-entry ?\' "\"")
      (setq user-ext-ebnf--init t)))

;;;###autoload
(add-hook 'ebnf-mode-hook #'ebnf--extra-hook)

(provide 'ebnf-ext)
;;; ebnf-ext.el ends here
