;;; imenu-ext --- IMenu extension.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun bind-imenu ()
  "Binds `imenu' to the right-mouse button locally.
The bindings are local to the active keymap, which means
buffers sharing the same major mode will be affected."
  (interactive)
  (if (not (boundp 'python-mode))
      (let ()
	(local-set-key (kbd "<mouse-3>") #'imenu)
	(message "Locally bound `imenu' to the right mouse button"))
    (user-error "`bind-imenu' is not meant to be used in Python mode")))

(defun bind-imenu-lsp ()
  "Binds `imenu' to the double left-click mouse button locally.
The bindings are local to the active keymap, which means
buffers sharing the same major mode will be affected."
  (interactive)
  (when (not (boundp 'python-mode))
    (setq mark-active nil)
    (local-set-key (kbd "<double-mouse-1>") #'imenu)
    (message "Bind `imenu' to the left mouse button double-clicked")))

;; (defun imenu--sh-hook ()
;;   (setq imenu-generic-expression
;; 	(list `("*Functions*"
;; 		,(rx line-start (* (syntax whitespace))
;; 		     "function" (+ (syntax whitespace))
;; 		     (group (* (syntax symbol)))
;; 		     (* (syntax whitespace)) (opt "()")) 1)
;; 	      `("*Functions*"
;; 		,(rx line-start (* (syntax whitespace))
;; 		     (group (* (syntax symbol)))
;; 		     (* (syntax whitespace)) (opt "()"))))))

(defun imenu--python-hook ()
  "Imenu Hook for Python mode."
  (setq imenu-generic-expression
	(list `("*Functions*"
		,(rx line-start (* (syntax whitespace))
		     (opt "async" (+ (syntax whitespace)))
		     "def" (+ (syntax whitespace))
		     (group (+ (syntax symbol)))
		     "(" (* nonl) ")"
		     (* nonl) ":")
		1))))

;; (add-hook 'sh-mode-hook #'imenu--sh-hook)
(add-hook 'python-mode-hook #'imenu--python-hook)

(provide 'imenu-ext)

;;; imenu-ext ends here
