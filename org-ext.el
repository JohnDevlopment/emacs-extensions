;;; org-ext --- Org mode extension.  -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-table)

(eval-when-compile
  (declare-function org-ext-custom-command "org-ext")
  (defvar org-ext-custom-command)
  (declare-function org-ext-insert-command "org-ext")
  (defvar org-ext-insert-command)
  (require 'cl-lib))

(require 'yasnippet)

;; (declare-function yas-ext-org-very-safe-expand "yasnippets-ext")
;; (load-extension "yasnippets-ext")

(define-prefix-command 'org-ext-custom-command)
(define-prefix-command 'org-ext-insert-command)
(make-variable-buffer-local 'yas-trigger-key)

;;; Variables

(defcustom user-ext-browse-url-brave-arguments nil
  "A list of strings to pass to Brave as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'user-extensions)

;;; Functions

(defun org-ext-open-url-at-point (&optional arg)
  "Open link, timestamp, footnote or tags at point.

When point is on a link, follow it.  Normally, files will be
opened by an appropriate application.  If the optional prefix
argument ARG is non-nil, Emacs will visit the file.  With
a double prefix argument, try to open outside of Emacs, in the
application the system uses for this file type."
  (interactive "P")
  (org-load-modules-maybe)
  (setq org-window-config-before-follow-link (current-window-configuration))
  (org-remove-occur-highlights nil nil t)
  (unless (run-hook-with-args-until-success 'org-open-at-point-functions)
    (let* ((context
	    ;; Only consider supported types, even if they are not the
	    ;; closest one.
	    (org-element-lineage
	     (org-element-context)
	     '(clock comment comment-block footnote-definition
		     footnote-reference headline inline-src-block inlinetask
		     keyword link node-property planning src-block timestamp)
	     t))
	   (type (org-element-type context)))
      (cond
       ((not type) (user-error "No link found"))
       ;; Do nothing on white spaces after an object.
       ((>= (point)
	    (save-excursion
	      (goto-char (org-element-property :end context))
	      (skip-chars-backward " \t")
	      (point)))
	(user-error "No link found"))
       ((eq type 'link)
	(let (protocol path)
	  (setq protocol (org-element-property :type context)
		path (org-element-property :path context))
	  (browse-url-brave (format "%s:%s" protocol path)
			    arg)))
       (t (user-error "No link found")))))
  (run-hook-with-args 'org-follow-link-hook))

;;;###autoload
(defun browse-url-brave (url &optional _new-window)
  "Browse URL in Brave browser.
_NEW-WINDOW is ignored."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "brave " url) nil
	   "brave-browser"
	   url
	   user-ext-browse-url-brave-arguments)))

(defconst org-ext-list-num-bullet-regexp
  "^\\([ \\t]*\\)\\([1-9][0-9]*\\)\\."
  "Regular expression for matching numered list items.

Group 1 matches the leading spaces.
Group 2 matches the number.")

(defconst org-ext-list-unordered-regexp
  "^\\([ \\t]*\\)-\\([ \\t]*\\[ ]\\)?"
  "Regular expression for matching unordered list items.

Group 1 matches the leading spaces.
Group 2 optionally matches a checkbox.")

(defun org-ext-list-shift-return ()
  "If point is in inside a list, enter a newline and add a list item.
Otherwise, call `org-return'."
  (interactive)
  (let* ((context (org-element-lineage
		   (org-element-context)
		   '(plain-list) t))
	 (type (org-element-type context))
	 bullet checkbox)
    (if (eq type 'plain-list)
	(let* ((list-type (org-element-property :type context))
	       (struct (org-element-property :structure context))
	       (element (car (last struct))))
	  (cond
	   ((eq list-type 'unordered)
	    ;; Unordered list
	    (setq checkbox (nth 4 element))
	    (cl-check-type checkbox string-or-null)
	    (org-list-insert-item (point) struct
				  (org-list-prevs-alist struct)
				  checkbox)
	    (org-end-of-line))
	   ((eq list-type 'ordered)
	    ;; Ordered list
	    (setq bullet (nth 2 element)
		  checkbox (nth 4 element))
	    (message "bullet: %S\nelement: %S" bullet element)
	    (message "checkbox: %S" checkbox)
	    (save-match-data
	      (if (and (stringp bullet)
		       (string-match
			org-ext-list-num-bullet-regexp
			bullet))
		  (let ((num (string-to-number (match-string 2 bullet))))
		    (message "num: %d" num)
		    (setq bullet (format "%d." (1+ num))))))
	    (org-list-insert-item (point) struct
				  (org-list-prevs-alist struct)
				  checkbox)
	    (org-end-of-line)
	    (org-ctrl-c-ctrl-c))))
      (org-return))))

(defun org-ext-list-headlines ()
  "List headlines in current buffer."
  (interactive)
  (occur "^\\*\\{1,3\\} .+"))

(defun org-ext--babel-confirm-babel-evalutate (lang _body)
  "Used by `org-babel-confirm-evaluate' to confirm Babel evaluation.
If this returns non-nil, Babel evaluation proceeds.
Otherwise, confirm.  LANG is the language name of the Babel
block.  _BODY is the body of the code block to be evaluated.
It is ignored.  See `org-babel-confirm-evaluate' for
details."
  (print lang)
  (not (string= lang "plantuml")))

;;;###autoload (autoload 'org-ext-scratch "org-ext" "Create an `org-mode' scratch buffer." t)
(define-scratch-buffer-function org-ext-scratch "org scratch" ()
  "Create an `org-mode' scratch buffer."
  nil
  (org-mode))

(defun org-ext--insert-commas (num-string)
  "Insert commas as thousands separators into NUM-STRING."
  (let* ((parts (split-string num-string "\\."))
	 (int-part (car parts))
	 (dec-part (cadr parts))
	 (int-with-commas
	  (reverse
	   (mapconcat 'identity
		      (seq-partition (reverse int-part) 3) ","))))
    (if dec-part
	(concat int-with-commas "." dec-part)
      int-with-commas)))

(defun org-ext-format-currency (num &optional prefix)
  "Return a formatted string with commas as thousands separators.
NUM is a number.  PREFIX is a string that will be prepended
to the result.  It is expected to contain a currency symbol."
  (let ((nstr (org-ext--insert-commas (format "%.2f" num))))
    (concat prefix nstr)))

(defun org-ext-scan-number (string)
  "Scan STRING for a number."
  (string-to-number
   (replace-regexp-in-string "[^0-9.]" "" string)))

(defun org-ext-scan-numbers (&rest strings)
  "Scan STRINGS for numbers and return them as a list."
  (mapcar #'org-ext-scan-number strings))

;;;###autoload
(defun org-ext-tbl-minor-mode (&optional arg)
  "Turn on `orgtbl-mode'.
ARG is passed to the function."
  (interactive "P")
  (orgtbl-mode (or arg 'toggle)))

(local-set-key (kbd "M-e") #'yas-expand)

(define-key org-mode-map (kbd "C-c M-c") #'org-ext-custom-command)
(define-key org-ext-custom-command "<tab>"
  (lambda ()
    "Call `org-set-startup-visibility'."
    (interactive)
    (org-set-startup-visibility)))

(local-set-key (kbd "C-c i") #'org-ext-insert-command)
(define-key org-ext-insert-command "h"
  (lambda ()
    "Insert a horizontal rule."
    (interactive)
    (insert "----------\n")))

(define-key org-mode-map (kbd "<S-return>") #'org-ext-list-shift-return)
(define-key org-mode-map (kbd "C-c o") #'org-ext-open-url-at-point)
(define-key org-mode-map (kbd "<C-tab>") #'tab-next)
(define-key org-mode-map (kbd "M-F") #'fill-region)

;; Hooks

;;;###autoload
(defun org--extra-hook ()
  "Extra hook for `org-mode'."
  (add-hook 'org-tab-first-hook #'yas-ext-org-very-safe-expand)
  (define-key yas-keymap [tab] #'yas-next-field))

;;;###autoload
(add-hook 'org-mode-hook #'org--extra-hook)

(provide 'org-ext)

;;; org-ext ends here
