;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")
(extension-check-requires yasnippet)

(require 'org)
(require 'org-element)
(require 'org-table)
(require 'yasnippet)

(eval-when-compile
  (declare-function org-ext-custom-command "org-ext")
  (declare-function org-ext-insert-command "org-ext")
  (defvar org-ext-insert-command)
  (defvar org-ext-custom-command)
  (require 'cl-lib)
  (require 'cl-ext)
  (require 'debug-ext))

(embed-doc-document-symbol org-ext
  "An extension to Org mode.

In addition to the exported commands, there are some changes
to the core Org mode functionality.

Firstly, with the addition of package `org-special-block-extras',
new special blocks have been added.  Among them, a block created by
me was added: admonition.

In order to insert it,
do \\[org-insert-structure-template] and press TAB, then
type the name of the block you want, e.g., \"aside\".  The
block supports the :header keyword, to specify a header for
the block in the HTML output."
  :commands
  browse-url-brave
  org-ext-enable-export-on-save
  org-ext-list-headlines
  org-ext-list-shift-return
  org-ext-open-url-at-point
  org-ext-tbl-minor-mode)

(define-prefix-command 'org-ext-custom-command)
(define-prefix-command 'org-ext-insert-command)
(make-variable-buffer-local 'yas-trigger-key)


;; ### Customization

(defgroup org-ext nil
  "Org Mode Extension."
  :group 'user-extensions)

(defcustom user-ext-org-browse-url-brave-arguments nil
  "Arguments to pass to Brave."
  :type '(repeat (string :tag "Argument"))
  :group 'org-ext)

(defface user-ext-org-deprecated
  '((t (:inherit org-todo)))
  "Face for outdated items."
  :group 'org-ext)


;; ### Variables

(defvar-local user-ext-org-babel-safe-eval nil
  "If non-nil, do not query user before evaluating block.")
(put 'user-ext-org-babel-safe-eval 'safe-local-variable #'booleanp)

(defvar-local user-ext-org--format-hooks nil)

(defvar-local user-ext-org-local-variables nil
  "Local table of variables for use in macros.")

(defconst user-ext-org-local-vars-function-name-symbol
  'user-ext-org--function)


;; ### Functions

(defun org-ext-set-startup-visibility ()
  "Call `org-set-startup-visibility'."
  (interactive)
  (emacs-version-cond-when-compile
    ((>= "28")
     (org-cycle-set-startup-visibility))
    (t (org-set-startup-visibility))))

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
	   user-ext-org-browse-url-brave-arguments)))

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
	    (org-ctrl-c-ctrl-c))
	   ((eq list-type 'descriptive)
	    (let ((def (read-string "Definition: ")))
	      ;; Definition list
	      (when (not (string-empty-p def))
		(org-list-insert-item (point) struct
				      (org-list-prevs-alist struct)
				      nil (format "%s :: " def))
		(org-end-of-line))))
	   (t
	    ;; Unknown
	    (--print-expr var context)
	    (error "Unknown list type: %s" list-type))))
      (org-return))))

(defun org-ext--complete-save-command ()
  (let* ((choices '("html" "pdf" "markdown"))
	 (format (completing-read "Format: " choices)))
    (pcase format
      ("html"
       (list #'org-html-export-to-html (upcase format)))
      ("pdf"
       (list #'org-latex-export-to-pdf (upcase format)))
      ("markdown"
       (list #'org-md-export-to-markdown (upcase format)))
      (_
       (user-error "Invalid choice %S" format)))))

(defsubst org-ext-format-save-export-p (format)
  "Return non-nil if FORMAT is set for export on save."
  (cl-member format user-ext-org--format-hooks :test #'string=))

(defun org-ext-enable-export-on-save (command format)
  "Enable exporting the document on save."
  (interactive (org-ext--complete-save-command))
  (cl-check-type command symbol)
  (cl-check-type format string)
  (unless (eq major-mode #'org-mode)
    (user-error "Must be in Org mode"))
  (unless (org-ext-format-save-export-p format)
    (add-hook 'after-save-hook
	      (lambda ()
		(and (y-or-n-p (format "Export to %s?" format))
		     (funcall command)))
	      nil 'local)
    (cl-pushnew format user-ext-org--format-hooks))
  (run-with-idle-timer 1 nil
		       #'message
		       "Auto-export %s file after save"
		       format))

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


;; --- Local Variables

(defun org-ext-local-vars-init ()
  "Initialize local vars system."
  (unless user-ext-org-local-variables
    (setq user-ext-org-local-variables (make-hash-table :test #'equal))))

(defmacro org-ext-local-vars-assert-init ()
  "Assert that the local vars system was initialized."
  (declare (debug t))
  `(unless user-ext-org-local-variables
     (error "Call `org-ext-local-vars-init' before calling this `%S'"
	    ,user-ext-org-local-vars-function-name-symbol)))

(defmacro org-ext-local-vars--with-function-name (fn &rest body)
  (declare (indent 1)
	   (debug (symbol &rest form)))
  (let ((fname user-ext-org-local-vars-function-name-symbol))
    `(let ((,fname ',fn))
       ,@body)))

(defun org-ext-local-vars-get (name)
  "Get the value associated with local variable NAME."
  (org-ext-local-vars--with-function-name org-ext-local-vars-get
    (org-ext-local-vars-assert-init)
    (gethash name user-ext-org-local-variables)))

(defun org-ext-local-vars-set (name value)
  "Set local variable NAME to VALUE."
  (org-ext-local-vars--with-function-name org-ext-local-vars-set
    (org-ext-local-vars-assert-init)
    (puthash name value user-ext-org-local-variables)))


;; --- Babel

(defun org-ext-babel-confirm-evaluate (lang body)
  (let ((ask t))
    (pcase lang
      ((or "emacs-lisp" "elisp")
       (with-temp-buffer
	 (emacs-lisp-mode)
	 (princ body (current-buffer))
	 (hack-local-variables 'nomode)
	 (setq ask (not user-ext-org-babel-safe-eval))))
      (_ t))
    ask))


;; --- Custom special blocks

(--ignore
  (autoload 'org-defblock "org-special-block-extras" nil nil t)
  (fmakunbound 'org--create-defmethod-of-defblock)
  (require 'org-special-block-extras)
  (org-defblock
   admonition (type nil title nil) nil
   "Enclose text in a box denoting an admonition.

== HTML ==

In HTML, this creates a \\=`section' element with the class
admonition-TYPE, where TYPE is the main arg. Under it is a
\\=`h3'--TYPE and TITLE are included (the latter only if it
is specified).

By default, no styling is applied; it has to be done
manually.  This can be done with a stylesheet applying a
style to the right class.

\(fn TYPE [:title TITLE]\)"
   (pcase backend
     ('html
      (let* ((prefix (capitalize (or type "note")))
	     (title (if title (format "%s: %s" prefix title)
		      prefix)))
	(format "<section class=\"admonition-%s\"><h3>%s</h3>%s</section>"
		type title contents)))))
  t)

(--ignore
  (defun org-ext--special-block (block-type str contents header)
    (let* ((raw-heading (plist-get header :heading))
	   (heading (if raw-heading (concat "<h3>" (org-strip-quotes raw-heading) "</h3>\n")
		      "")))
      (format "<%s%s>\n%s%s\n</%s>" block-type str heading contents block-type)))

  (fext-replace-function org-html-special-block "org-ext" (special-block contents info)
    "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
    :remove
    (let* ((block-type (org-element-property :type special-block))
           (html5-fancy (and (org-html--html5-fancy-p info)
                             (member block-type org-html-html5-elements)))
           (attributes (org-export-read-attribute :attr_html special-block)))
      (unless html5-fancy
	(let ((class (plist-get attributes :class)))
          (setq attributes (plist-put attributes :class
                                      (if class (concat class " " block-type)
					block-type)))))
      (let* ((contents (or contents ""))
	     (name (org-element-property :name special-block))
	     (a (org-html--make-attribute-string
		 (if (or (not name) (plist-member attributes :id))
		     attributes
		   (plist-put attributes :id name))))
	     (str (if (org-string-nw-p a) (concat " " a) "")))
	(if html5-fancy
	    (cl-ext-progn
	      (pcase block-type
		((or "aside" "section")
		 (org-ext--special-block
		  block-type str contents
		  (org-export-read-attribute :header special-block)))
		(_
		 (format "<%s%s>\n%s</%s>" block-type str contents block-type))))
	  (format "<div%s>\n%s\n</div>" str contents)))))
  t)


;; --- Keymaps

(keymaps-ext-set-keymap org-mode-map "M-e" #'yas-expand)

(keymaps-ext-set-keymap org-mode-map "C-c M-c" #'org-ext-custom-command)
(keymaps-ext-set-keymap org-ext-custom-command "<tab>" #'org-ext-set-startup-visibility)

(keymaps-ext-set-keymap org-mode-map "C-c i" #'org-ext-insert-command)
;; (keymaps-ext-set-keymap org-ext-insert-command "h"
;; 	    (lambda ()
;; 	      "Insert a horizontal rule."
;; 	      (interactive)
;; 	      (insert "----------\n")))

(keymaps-ext-set-keymap org-mode-map "<S-return>" #'org-ext-list-shift-return)
(keymaps-ext-set-keymap org-mode-map "C-c o" #'org-ext-open-url-at-point)
(keymaps-ext-set-keymap org-mode-map "<C-tab>" #'tab-next)
(keymaps-ext-set-keymap org-mode-map "M-F" #'fill-region)
(keymaps-ext-set-keymap org-mode-map "C-c c @" #'org-mark-ring-goto)

(keymaps-ext-set-keymap orgtbl-mode-map "C-c TAB" #'org-table-toggle-column-width)

(easy-menu-define user-ext-org-menu-map org-mode-map
  "Org menu."
  '("Org Extension"
    ["Open URL At Point" org-ext-open-url-at-point]
    ["Enable Export On Save" org-ext-enable-export-on-save]
    "--"
    ["Inline Images" org-toggle-inline-images
     :style toggle :selected org-inline-image-overlays]))

(--ignore
  (defvar user-ext-org-temp-map (make-sparse-keymap))

  (define-key-after (lookup-key org-mode-map
				[menu-bar Org Hyperlinks])
    (easy-menu-binding (lookup-key org-mode-map
				   [menu-bar Org Hyperlinks])
		       "--")
    '(nil menu-item "--")
    'Descriptive\ Links)

  (easy-menu-define-key
   (lookup-key org-mode-map
	       [menu-bar Org Hyperlinks])
   nil
   '(menu-item "--"))

  (easy-menu-define-key
   (lookup-key org-mode-map
	       [menu-bar Org Hyperlinks])
   org-toggle-inline-images)

  (prog1 nil
    (with-current-buffer (get-buffer-create "*output*")
      (emacs-lisp-mode)
      (cl-prettyprint (lookup-key org-mode-map
				  [menu-bar Org Hyperlinks]))
      (run-with-idle-timer 0.5 nil #'activate-view-mode 1)
      (set-buffer-modified-p nil))
    (pop-to-buffer "*output*" t))

  ;; <menu-bar> <Org> <Hyperlinks> <Descriptive Links>
  
  (easy-menu-add-item
   org-mode-map
   '("Hyperlinks")
   ["Up Subtree" outline-up-heading])
  t)


;; ### Hooks

;;;###autoload
(defun org--extra-hook ()
  "Extra hook for `org-mode'."
  t)

;; Yasnippet
(add-hook 'org-tab-first-hook #'yas-ext-org-very-safe-expand)
(keymaps-ext-set-keymap yas-keymap "<tab>" #'yas-next-field)

;;;###autoload
(add-hook 'org-mode-hook #'org--extra-hook)

(extension-provide 'org-ext)
;;; org-ext ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-org")
;; eval: (abbrev-ext-define-local-abbrev "ox" "org-ext")
;; End:
