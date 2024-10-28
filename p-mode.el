;;; -*- lexical-binding: t; -*-

(defcustom user-ext-electric-pair-global-modes '(not)
  "List of global modes in which `electric-pair-local-mode' should be
enabled."
  :type '(choice
	  (const :tag "all" t)
	  (set :tag "modes" :value (not)
	       (const :tag "Except" not)
	       (repeat :inline t (symbol :tag "Mode"))))
  :group 'user-ext-global-modes)

(defun electric-pair-mode-turn-on ()
  "Turn on electric pair mode"
  (interactive)
  (when (cond ((eq (car-safe user-ext-electric-pair-global-modes) 'not)
	       (not (memq major-mode (cdr user-ext-electric-pair-global-modes))))
	      (t
	       (memq major-mode user-ext-electric-pair-global-modes)))
    (electric-pair-local-mode 1)))

(define-globalized-minor-mode global-electric-pair-mode electric-pair-mode electric-pair-mode-turn-on)
