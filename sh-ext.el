;; -*- lexical-binding: t; -*-

;;; Code:

(require 'ido)
(require 'sh-script)

(defconst sh-ext-function-regex
  "^\\(?:function\\s-+\\([A-Z_a-z-]+\\)\\|\\(?1:[A-Z_a-z-]+\\)()\\)"
  "Regular expression for finding functions.")

(defun sh-ext-occur-functions (&optional nlines)
  "Run `occur' on the current buffer for function definitions.
Each line is matched with `sh-ext-function-regex'.

Optional arg NLINES is interpreted the same way as for
`occur'.  Interactively it is the prefix arg.

Optional args BEG and END, are used to restrict search to a
designated region.  Interactively, the region is used to set
these args."
  (interactive "p")
  (let (beg end)
    (when (use-region-p)
      (setq beg (region-beginning) end (region-end)))
    (occur sh-ext-function-regex
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

(defconst user-ext-sh-ext-color-escapes
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
    (reset . "0"))
  "Color/property names mapped to their equivalent Bash escape code.")

(defun sh-ext--color-escape-complete ()
  (list
   (intern (ido-completing-read "Color escape: "
				(mapcar (lambda (l)
					  (symbol-name (car l)))
					user-ext-sh-ext-color-escapes)
				nil t))
   current-prefix-arg))

(defun sh-ext-color-escape (color &optional arg)
  "Insert a Bash color escape at point.

COLOR is a symbol denoting the name of the color to
use.  Valid names can be found in
`user-ext-sh-ext-color-escapes'.

When called interactively, ARG is the prefix argument.  If
it is non-nil, \"1;\" is prepended to the color code."
  (interactive (sh-ext--color-escape-complete))
  (let (result)
    (setq result
	  (if arg
	      (format "\\e[1;%sm" (cdr (assq color user-ext-sh-ext-color-escapes)))
	    (format "\\e[%sm" (cdr (assq color user-ext-sh-ext-color-escapes)))))
    (insert result)))

(define-scratch-buffer-function sh-ext-scratch-buffer "sh scratch" nil
  "Open a scratch buffer to edit Shell script."
  nil
  (sh-mode))

(defun sh-ext-insert-non-printing-escape ()
  "Insert Bash non-printing escape sequence."
  (interactive)
  (insert "\\[\\]")
  (left-char 2))

;; Hook

(defun sh--extra-hook ()
  (setq imenu-generic-expression
	'(("*Functions*" "^\\s-**function\\s-+\\([_a-z-]+\\)" 1)
	  ("*Functions*" "^\\s-*\\([_a-z-]+\\)()" 1)
	  ("*Aliases*" "\\<alias \\([A-Za-z0-9_-]+\\)" 1)
	  ("*Tags*" "^\\s-*###\\s-*\\(.+\\)" 1))))

(add-hook 'sh-mode-hook #'sh--extra-hook)

;; Key bindings and abbrevs
(eval-and-compile

  (define-abbrev sh-mode-abbrev-table "cmds" "" #'sh-ext-skeleton-src-command-list)
  (define-key sh-mode-map (kbd "C-c \\") #'sh-ext-color-escape)
  (define-key sh-mode-map (kbd "C-c [") #'sh-ext-insert-non-printing-escape))
