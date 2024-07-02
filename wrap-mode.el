;; -*- lexical-binding: t; -*-

(defcustom user-ext-visual-line-global-modes '(not)
  "List of global modes in which `visual-line-mode' should be enabled."
  :type '(choice
	  (const :tag "all" t)
	  (set :tag "modes" :value (not)
	       (const :tag "Except" not)
	       (repeat :inline t (symbol :tag "Mode"))))
  :group 'user-ext-global-modes)

(defun visual-line-mode-turn-on ()
  "Turn on visual line mode (AKA, line wrap)."
  (interactive)
  (when (cond ((eq (car-safe user-ext-visual-line-global-modes) 'not)
	       (not (memq major-mode (cdr user-ext-visual-line-global-modes))))
	      (t
	       (memq major-mode user-ext-visual-line-global-modes)))
    (visual-line-mode t)))

(define-globalized-minor-mode global-visual-line-mode visual-line-mode visual-line-mode-turn-on)
