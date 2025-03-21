;; -*- lexical-binding: t; -*-

(require 'ido)
(require 'sh-script)
(require 'cl-ext)

(eval-when-compile
  (require 'cl-lib)
  (defvar user-ext-sh-fold-map))

;; Variables

(define-abbrev global-abbrev-table "NULL" "null" #'abbrev-ext-insert-hook :system t)

(defconst user-ext-sh-function-regex
  (rx bol (* (syntax whitespace))
      (group "function" (+ (syntax whitespace))
	     (group (+ (any "A-Za-z0-9" ?_ ?-))))
      (* (syntax whitespace)) ?{)
  "Regular expression for finding functions.
Group 1 matches the first line of the declaration starting
with the keyword 'function'.
Group 2 matches the name of the function.")

(defconst user-ext-sh-color-escapes
  '((fg-black . "30")
    (bg-black . "40")
    (fg-red . "31")
    (bg-red . "41")
    (fg-green . "32")
    (bg-green . "42")
    (fg-brown . "33")
    (bg-brown . "43")
    (fg-blue . "34")
    (bg-blue . "44")
    (fg-purple . "35")
    (bg-purple . "45")
    (fg-cyan . "36")
    (bg-cyan . "46")
    (fg-light-gray . "37")
    (bg-light-gray . "47")
    (reset . "00"))
  "Color/property names mapped to their equivalent Bash escape code.")

;; Functions

(defun sh-ext--fold-map-prompt ()
  (format "Sh Fold: %s - hide function, %s - hide all functions, %s - show function"
	  (propertize "f" 'face 'font-lock-constant-face)
	  (propertize "C-f" 'face 'font-lock-constant-face)
	  (propertize "M-f" 'face 'font-lock-constant-face)))

(defun sh-ext--hidden (&optional position)
  "Return non-nil if point is in an already hidden block.
If POSITION is non-nil, move point to that position before
the check."
  (cl-check-type position (or integer-or-marker null))
  (cl-assert (and (boundp 'hs-minor-mode) hs-minor-mode))
  (cl-ext-save-point
    (and position (goto-char position))
    (or (hs-already-hidden-p)
	(progn (left-char)
	       (hs-already-hidden-p)))))

(defun sh-ext--looking-at (what &optional position)
  "Return t if text point matches a pattern for form WHAT.
WHAT is a symbol indicating the form we want to match.

WHAT is one of the following:
'function   Matches by regular expression
            `user-ext-sh-function-regex'"
  (cl-check-type position (or integer-or-marker null))
  (cl-ext-save-point
    (and position (goto-char position))
    (pcase what
      ('function
       (looking-at user-ext-sh-function-regex)))))

(defun sh-ext-show-function ()
  "Show the hidden function at point."
  (interactive)
  (elisp-ext-enable-hs-minor-mode)
  (let ((buffer-read-only t)
	(bol (line-beginning-position))
	pos)
    (when (and (sh-ext--looking-at 'function bol)
	       (setq pos (match-end 0))
	       (sh-ext--hidden pos))
      (goto-char pos)
      (hs-show-block))))

(defun sh-ext-hide-all-functions ()
  "Hide all functions in buffer."
  (interactive)
  (elisp-ext-enable-hs-minor-mode)
  (let ((tl (syntax-ppss-toplevel-pos (syntax-ppss)))
	(buffer-read-only t))
    (cl-ext-when tl
      (goto-char tl))
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (search-forward-regexp user-ext-sh-function-regex nil t)
	       do
	       (hs-hide-block 4)))))

(defun sh-ext-hide-function ()
  "Hide the surrounding function."
  (interactive)
  (elisp-ext-enable-hs-minor-mode)
  (let* ((ppss (syntax-ppss))
	 (tl (syntax-ppss-toplevel-pos ppss))
	 (buffer-read-only t)
	 (pos (point)))
    (when tl
      (goto-char tl)
      (right-char))
    (goto-char (line-beginning-position))
    (if (looking-at user-ext-sh-function-regex)
	(prog1 (match-end 0)
	  (goto-char (match-end 0))
	  (setq pos (match-beginning 0))
	  (hs-hide-block)
	  (goto-char pos))
      (goto-char pos))))

(defun sh-ext-mark-function ()
  "Mark the surrounding function."
  (interactive)
  (let* ((ppss (syntax-ppss))
	 (tl (syntax-ppss-toplevel-pos ppss)))
    (goto-char tl)
    (beginning-of-line)
    (when (looking-at user-ext-sh-function-regex)
      (goto-char (match-beginning 1))
      (activate-mark)
      (goto-char tl)
      (forward-sexp))))

(defun sh-ext-occur-functions (&optional nlines)
  "Run `occur' on the current buffer for function definitions.
Each line is matched with `user-ext-sh-function-regex'.

Optional arg NLINES is interpreted the same way as for
`occur'.  Interactively it is the prefix arg.

Optional args BEG and END, are used to restrict search to a
designated region.  Interactively, the region is used to set
these args."
  (interactive "p")
  (let (beg end)
    (when (use-region-p)
      (setq beg (region-beginning) end (region-end)))
    (occur user-ext-sh-function-regex
	   (if (= nlines 1) -2 nlines)
	   (if (use-region-p)
	       (list beg end)))
    (with-current-buffer "*Occur*"
      ;; Buffer defined = kill buffer
      (let* ((bufname "*Occur: sh functions*")
	     (buf (get-buffer bufname)))
	(when buf
	  (kill-buffer bufname))
	(highlight-parentheses-mode -1)
	(rename-buffer bufname)
	(local-set-key "q" #'kill-and-quit)))))

(defun sh-ext--once-mods-hist (c)
  (cond
   ((string= c "o")
    " (once)")
   ((string= c "h")
    " (once, modifies history)")
   (t nil)))

(defun sh-ext--color-escape-complete ()
  (list
   (intern (ido-completing-read "Color escape: "
				(mapcar (lambda (l)
					  (symbol-name (car l)))
					user-ext-sh-color-escapes)
				nil t))
   current-prefix-arg))

(defun sh-ext-color-escape (color &optional arg)
  "Insert a Bash color escape at point.

COLOR is a symbol denoting the name of the color to
use.  Valid names can be found in
`user-ext-sh-color-escapes'.

When called interactively, ARG is the prefix argument.  If
it is non-nil, \"1;\" is prepended to the color code."
  (interactive (sh-ext--color-escape-complete))
  (let (result)
    (setq result
	  (if arg
	      (format "\\e[1;%sm" (cdr (assq color user-ext-sh-color-escapes)))
	    (format "\\e[%sm" (cdr (assq color user-ext-sh-color-escapes)))))
    (insert result)))

(defun sh-ext-insert-non-printing-escape ()
  "Insert Bash non-printing escape sequence."
  (interactive)
  (insert "\\[\\]")
  (left-char 2))

(define-skeleton sh-ext-skeleton-src-command-list
  "Inserts a command list like is seen in \"src\"."
  "Delimter: "
  "cat <<" str \n
  "Commands:"\n
  ("Command: "
   "  * " str (sh-ext--once-mods-hist (skeleton-read
				       "Character (o, h): "))
   "\n")
  str)

;; Hook

;;;###autoload
(defun sh--extra-hook ()
  (setq imenu-generic-expression
	'(("*Functions*" "^\\s-**function\\s-+\\([_a-z-]+\\)" 1)
	  ("*Functions*" "^\\s-*\\([_a-z-]+\\)()" 1)
	  ("*Aliases*" "\\<alias \\([A-Za-z0-9_-]+\\)" 1)
	  ("*Tags*" "^\\s-*###\\s-*\\(.+\\)" 1))))

;;;###autoload
(add-hook 'sh-mode-hook #'sh--extra-hook)

;; Key bindings and abbrevs
(define-abbrev sh-mode-abbrev-table "cmds" "" #'sh-ext-skeleton-src-command-list)
(define-key sh-mode-map (kbd "C-c \\") #'sh-ext-color-escape)
(define-key sh-mode-map (kbd "C-c [") #'sh-ext-insert-non-printing-escape)

(define-prefix-command 'user-ext-sh-fold-map nil (sh-ext--fold-map-prompt))
(define-key sh-mode-map (kbd "C-c f") 'user-ext-sh-fold-map)
(define-key user-ext-sh-fold-map (kbd "f") #'sh-ext-hide-function)
(define-key user-ext-sh-fold-map (kbd "C-f") #'sh-ext-hide-all-functions)
(define-key user-ext-sh-fold-map (kbd "M-f") #'sh-ext-show-function)

(provide 'sh-ext)

;;; sh-ext ends here
