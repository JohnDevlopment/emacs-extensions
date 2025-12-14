;; -*- lexical-binding: t; -*-

(require 'css-mode)

(use-package company-capf
  :commands
  company-capf)

(use-package generator
  :autoload
  iter-defun
  iter-next
  iter-yield)


;; ### Customization

(defgroup css-ext nil
  "CSS extension."
  :group 'user-extensions)


;; ### Functions

(iter-defun css-ext-iter-colors ()
  "Construct an iterator to CSS's named colors.
On each iteration, yield a cons (NAME . COLOR), where NAME
is a string denoting the name of a color and COLOR the hex
RGB value.

The iterator returned by this function can be used with
`iter-next', `iter-do', and so on.  See the info node
`(elisp)Generators' for more information.

When the iterator is exhausted, nil is returned."
  (dolist (elt css--color-map)
    (iter-yield elt)))


;; ### Color Pick Mode

(defconst css-ext-color-picker-name-field-width
  (eval-when-compile
    (cl-loop for val iter-by (css-ext-iter-colors)
	     maximize
	     (-let (((name . _color) val))
	       (length name))))
  "String length of the longest CSS color name.")

(defconst css-ext-color-picker--register ?c)

(defun css-ext-color-picker ()
  "Open a display buffer for CSS named colors.

A list of CSS's named colors are displayed line by line.
If the user picks one of them via \\[css-ext-color-picker-pick],
the color name or its hex value are inserted into the buffer
in which this command was called."
  (interactive "*")
  (barf-if-buffer-read-only)
  (window-configuration-to-register css-ext-color-picker--register)
  (let ((buffer (get-buffer-create "*Css Colors*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(delete-region (point-min) (point-max))
	(css-ext-color-picker-mode)
	(iter-do (val (css-ext-iter-colors))
	  (-let (((name . color) val)
		 (fmt (s-lex-format "%-${css-ext-color-picker-name-field-width}s  %s\n")))
	    (insert (format fmt (propertize name 'face `(:background ,color))
			    color)))))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'css-ext-color-picker))
     (run-with-idle-timer 0.5 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t))
 t)

(defun css-ext-color-picker-pick (&optional arg)
  "Pick the color at point.
ARG is passed to `kill-and-quit'.
If point is in the whitespace between a color name and its
hex value, move point to the \"closest\" one."
  (interactive "P")
  (let ((tap
	 (or (thing-at-point 'symbol)
	     (if (< (current-column) css-ext-color-picker-name-field-width)
		 (cl-ext-progn
		   (beginning-of-line)
		   (thing-at-point 'symbol))
	       (end-of-line)
	       (forward-char -1)
	       (thing-at-point 'symbol)))))
    (or tap (user-error "Failed to get symbol at point"))
    (css-ext-color-picker-kill)
    (insert tap)))

(defun css-ext-color-picker-quit ()
  "Quit the buffer, restoring the previous buffer and position."
  (interactive)
  (quit-window)
  (jump-to-register css-ext-color-picker--register))

(defun css-ext-color-picker-kill ()
  "Kill the buffer, restoring the previous buffer and position."
  (interactive)
  (kill-and-quit t)
  (jump-to-register css-ext-color-picker--register))


;; --- Major Mode Definition

(defconst css-ext-color-picker-mode-map
  (make-composed-keymap nil special-mode-map))

(define-derived-mode css-ext-color-picker-mode special-mode
  "CSS Color Picker"
  "Minor mode for choosing a color.")

(modify-syntax-entry ?\# "_" css-ext-color-picker-mode-syntax-table)

(define-key css-ext-color-picker-mode-map (kbd "<return>") #'css-ext-color-picker-pick)
(define-key css-ext-color-picker-mode-map (kbd "q") #'css-ext-color-picker-quit)
(define-key css-ext-color-picker-mode-map (kbd "k") #'css-ext-color-picker-kill)

(define-key css-mode-map (kbd "S-M-SPC") #'company-capf)


;; ### Extension Hook

;;;###autoload
(defun css--extra-hook ()
  t)

;;;###autoload
(add-hook 'css-mode-hook #'css--extra-hook)

(provide 'css-ext)
;;; css-ext.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "cx" "css-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-css")
;; eval: (abbrev-ext-define-local-abbrev "cxcp" "css-ext-color-picker")
;; End:
