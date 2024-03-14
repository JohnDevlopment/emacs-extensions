(eval-and-compile
  (require 'org)
  (make-local-variable 'org-ext-bindings)
  (make-variable-buffer-local 'yas-trigger-key)
  (require 'debug-ext))

(defvar org-ext-bindings nil
  "Non-nil if `org-ext-bindings' was called.")

(defcustom user-ext-browse-url-brave-arguments nil
  "A list of strings to pass to Brave as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'user-extensions)

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
	   (type (org-element-type context))
	   (value (org-element-property :value context)))
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

(defun browse-url-brave (url &optional _new-window)
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

(defun org-ext-list-shift-return (&optional arg)
  "If point is in inside a list, enter a newline and add a list item.
Otherwise, call `org-return'."
  (interactive "p")
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
	    (setq checkbox (nth 4 element))
	    (assert `(or (stringp ',checkbox) (null ,checkbox)))
	    (org-list-insert-item (point) struct
				  (org-list-prevs-alist struct)
				  checkbox)
	    (org-end-of-line))
	   ((eq list-type 'ordered)
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

(defun org-ext--babel-confirm-babel-evalutate (lang body)
  "Used by `org-babel-confirm-evaluate' to confirm Babel evaluation.
If this returns non-nil, Babel evaluation proceeds. Otherwise, confirm."
  (print lang)
  (not (string= lang "plantuml")))

(defun org--extra-hook ()
  (org-ext-bindings)
  (setq yas-trigger-key [tab])
  (add-hook 'org-tab-first-hook #'yas-ext-org-very-safe-expand)
  (define-key yas-keymap [tab] #'yas-next-field))

(add-hook 'org-mode-hook #'org--extra-hook)

(defun org-ext-bindings ()
  "Bind <C-tab> to `tab-next'."
  (interactive)
  (when (not org-ext-bindings)
    (local-set-key (kbd "M-e") #'yas-expand)
    ;; Commands: C-c M-c ...
    (define-prefix-command #'org-mode-custom-command-prefix)
    (local-set-key (kbd "C-c M-c") #'org-mode-custom-command-prefix)
    (define-key org-mode-custom-command-prefix "<tab>"
      (lambda ()
	"Call `org-set-startup-visibility'."
	(interactive)
	(org-set-startup-visibility)))
    ;; Insertion commands: C-c i ...
    (define-prefix-command 'org-mode-custom-insert-command-prefix)
    (local-set-key (kbd "C-c i") #'org-mode-custom-insert-command-prefix)
    (define-key org-mode-custom-insert-command-prefix "h"
      (lambda ()
	"Insert a horizontal rule."
	(interactive)
	(insert "----------\n")))
    ;;
    (define-key org-mode-map (kbd "<S-return>") #'org-ext-list-shift-return)
    (define-key org-mode-map (kbd "C-c o") #'org-ext-open-url-at-point)
    (define-key org-mode-map (kbd "<C-tab>") #'tab-next)
    (define-key org-mode-map (kbd "M-F") #'fill-region)
    (setq org-ext-bindings t)))
