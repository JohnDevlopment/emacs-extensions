;; -*- lexical-binding: t; -*-

(require 'alist-ext)
(require 'mhtml-mode)
(require 'ido)
(require 'sgml-mode)
(require 'tempo)

(eval-when-compile
  (require 'debug-ext))

(use-package org-macs
  :autoload
  org-split-string)

(use-package lsp-mode
  :autoload
  lsp-with-undo-amalgamate)

(--declare (debug-level 1))


;; ### Customization

(defgroup mhtml-ext nil
  "Edit with HTML + CSS/JS."
  :group 'user-extensions)


;; ### Variables

(defconst user-ext-mhtml-entities
  (alist-ext-define "QUOTATION MARK (\")" "&quot;"
		    "AMPERSAND (&)" "&amp;"
		    "LESS-THAN SIGN (<)" "&lt;"
		    "GREATER-THAN SIGN (>)" "&gt;"
		    "NO-BREAK SPACE" "&nbsp;"
		    "CENT SIGN ¢" "&cent;"
		    "POUND SIGN (£)" "&pound;"
		    "CURRENCY SIGN (¤)" "&curren;"
		    "YEN SIGN (¥)" "&yen;"
		    "BROKEN BAR (¦)" "&brvbar;"
		    "SECTION SIGN (§)" "&sect;"
		    "COPYRIGHT SIGN (©)" "&copy;"
		    "LEFT-POINTING DOUBLE ANGLE QUOTATION MARK («)" "&laquo;"
		    "SOFT HYPHEN" "&shy;"
		    "REGISTERED SIGN (®)" "&reg;"
		    "MACRON (¯)" "&macr;"
		    "DEGREE SIGN (°)" "&deg;"
		    "PLUS-MINUS SIGN (±)" "&plusmn;"
		    "BALLOT BOX WITH CHECK (☑)" "&#9745;"
		    "BALLOT BOX WITH X (☒)" "&#9746;"))

(defconst user-ext-mhtml-tag-alist
  (alist-ext-define
   "p" ">"
   "section" "\n>"
   "ul" ">\n"
   "input" "/"
   "li" ">")
  "An alist of HTML tags to their configuration flags.
Each element takes the form (TAG . FLAGS)
TAG is the name of a tag.  FLAGS is a list of flags as a
string.

Flags / and > are mututally exclusive: / indicates that TAG
does not have an end tag (e.g., \"<input .../>\"), and >
indicates that it does have an end tag.")

(defconst user-ext-mhtml-special-tag-alist
  (alist-ext-define
   "table" #'mhtml-ext-insert-table
   "tr" #'mhtml-ext-insert-table-row)
  "An alist of so-called special tags.
A special tag is one that has its own dedicated function
instead of using the default command.
Each element has the form (TAG . FUNCTION).  FUNCTION is
called instead of `mhtml-ext-skeleton-tag'.")

(defvar user-ext-mhtml--flag nil)


;; ### Templates/Skeletons

(defmacro mhtml-ext-tempo-define-template (name doc elements)
  "Define a template NAME which inserts ELEMENTS.
The template will be called tempo-template-mhtml-ext-NAME.
DOCSTRING is the documentation string for the command.
ELEMENTS is a list of elements recognized by
`tempo-define-template', which see.

See also: `tempo-ext-tempo-handler'.

\(fn NAME DOCSTRING ELEMENTS)"
  (declare (indent defun) (doc-string 2)
	   (debug (&define name stringp sexp)))
  (cl-check-type name string)
  (cl-check-type doc string)
  (cl-check-type elements list)
  (let* ((name (format "mhtml-ext-%s" name))
	 (fname (format "tempo-template-%s" name)))
    `(prog1 ',(intern fname)
       (tempo-define-template ,name ',elements nil ,doc))))


;; --- Skeletons

(defmacro mhtml-ext-define-skeleton (name doc &rest skel)
  "Define a skeleton NAME which inserts SKELETON.
The command will be called mhtml-ext-skeleton-NAME.
DOCSTRING will be its documentation.

\(fn COMMAND DOCSTRING SKELETON...)"
  (declare (debug (&define name stringp skeleton-edebug-spec))
	   (indent defun) (doc-string 2))
  (cl-check-type name symbol)
  (cl-check-type doc string)
  (let ((fname (intern (format "mhtml-ext-skeleton-%S" name))))
    `(prog1 ',fname
       (define-skeleton ,fname ,doc ,@skel))))

(mhtml-ext-define-skeleton tag--flag
  ""
  "Tag: "
  '(setq v1 user-ext-mhtml--flag)
  `(nil (when (s-contains-p "\n" ,v1)
	  '\n)
	(if (s-contains-p "/" ,v1)
	    '(nil -1 " />")
	  '(nil "</" ,str ?> >))))

(mhtml-ext-define-skeleton tag
  "Insert a tag."
  ;; (funcall (or skeleton-transformation-function 'identity)
  ;;          (setq sgml-tag-last
  ;; 		 (mhtml-ext-read-tag)))
  nil
  ?< str
  `(nil '(setq user-ext-mhtml--flag
	       (alist-get ,str user-ext-mhtml-tag-alist "" nil #'string=))
	'(sgml-attributes ,str t) ?> _
	'(mhtml-ext-skeleton-tag--flag ,str)))
(--ignore
 (mhtml-ext-skeleton-tag)

 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'mhtml-ext-skeleton-tag))
     (run-with-idle-timer 0.5 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t))
 t)


;; ### Tree Sitter

(require 'tree-sitter)
(or (featurep 'tree-sitter-ext)
    (load-extension "tree-sitter-ext"))

(tree-sitter-ext-set-mode mhtml-ext-tree-sitter-mode)

(defvar mhtml-ext-tree-sitter--query-cursor nil)

(defun mhtml-ext-tree-sitter--setup ()
  (unless mhtml-ext-tree-sitter--query-cursor
    (setq mhtml-ext-tree-sitter--query-cursor (tsc-make-query-cursor))))

(defun mhtml-ext-tree-sitter--teardown ()
  (when mhtml-ext-tree-sitter--query-cursor
    (kill-local-variable 'mhtml-ext-tree-sitter--query-cursor)))

(define-minor-mode mhtml-ext-tree-sitter-mode
  "Syntax parsing of HTML mode buffers using tree sitter."
  :group 'mhtml-ext
  (unless (eq major-mode 'mhtml-mode)
    (mhtml-ext-tree-sitter-mode 0)
    (user-error "`mhtml-ext-tree-sitter-mode' only works in HTML mode"))
  (tree-sitter--handle-dependent mhtml-ext-tree-sitter-mode
    #'mhtml-ext-tree-sitter--setup
    #'mhtml-ext-tree-sitter--teardown))


;; ### Functions

(defun mhtml-ext--entities ()
  (cl-loop
   for i below (length user-ext-mhtml-entities)
   collect (car (nth i user-ext-mhtml-entities))))

;;;###autoload
(defun mhtml-ext-insert-entity (name)
  "Insert an entity symbol called NAME."
  (interactive (list (ido-completing-read "Name: " (mhtml-ext--entities))))
  (let ((entity (alist-get name user-ext-mhtml-entities)))
    (insert entity)))

(defun mhtml-ext-insert-tag (tag &optional arg interactive-p)
  "Insert TAG at point.

This runs the command returned by `mhtml-ext-get-tag-insert-function',
which see."
  (declare (interactive-only t))
  (interactive
   (cl-ext-progn
     (barf-if-buffer-read-only)
     (list (funcall (or skeleton-transformation-function 'identity)
		    (setq sgml-tag-last
			  (mhtml-ext-read-tag)))
	   current-prefix-arg
	   t)))
  (or interactive-p
      (error "This is an interactive-only function"))
  (barf-if-buffer-read-only)
  (let ((fn (mhtml-ext-get-tag-insert-function tag)))
    (if fn
	(call-interactively fn)
      (mhtml-ext-skeleton-tag tag arg))))

(defsubst mhtml-ext-get-tag-insert-function (tag)
  "Return the function to use for inserting TAG.
See `user-ext-mhtml-special-tag-alist'."
  (cl-check-type tag string)
  (alist-get tag user-ext-mhtml-special-tag-alist
	     nil
	     nil
	     #'string=))

(defun mhtml-ext-insert-table-row (type &optional columns)
  (interactive (let ((arg current-prefix-arg))
		 (barf-if-buffer-read-only)
		 (list (if arg 'heading 'normal)
		       (read-number "Columns: "))))
  (cl-check-type type symbol)
  (barf-if-buffer-read-only)
  (let* ((tag-name (pcase type
		     ('heading "th")
		     ('normal "td")
		     (_ (error "Invalid TYPE `%S' %S" type
			       '(heading normal)))))
	 (tag-start (format "<%s>" tag-name))
	 (tag-end (format "</%s>" tag-name))
	 (on-region (tempo-ext-on-region)))
    (mapc (##tempo-insert %1 on-region)
	  `("<tr>" r
	    (&repeat ,columns
		     \n ,tag-start ,tag-end >)
	    \n "</tr>" >))))

(defun mhtml-ext-insert-table (&optional size)
  "Insert a table with the given SIZE at point.
SIZE is a string Columns x Rows--for example \"3x2\"."
  (interactive "*")
  (barf-if-buffer-read-only)
  (unless size
    (setq size (let ((default "5x2"))
		 (read-string
		  (format "Table size Columns x Rows [e.g. %s] " default)
		  "" nil default))))
  (let* ((split (org-split-string size " *x *"))
	 (columns (string-to-number (car split)))
	 (rows (string-to-number (nth 1 split)))
	 (on-region (tempo-ext-on-region)))
    (mapc (##tempo-insert %1 on-region)
	  `("<table>"
	    (y-or-n-p "Add header row? " header)
	    (&if (flag header)
		 \n "<tr>" >
		 (&repeat ,columns
			  \n "<th></th>" >)
		 \n "</tr>" >)
	    (&repeat ,rows
		     \n "<tr>" >
		     (&repeat ,columns
			      \n "<td></td>" >)
		     \n "</tr>" >)
	    \n "</table>" >))))

(defun mhtml-ext-read-tag ()
  "Prompt the user for a tag."
  (completing-read
   (if (> (length sgml-tag-last) 0)
       (format "Tag (default %s): " sgml-tag-last)
     "Tag: ")
   sgml-tag-alist nil nil nil 'sgml-tag-history sgml-tag-last))

(defun mhtml-ext-insert-end-tag (&optional tag)
  "Insert an end tag."
  (interactive (let ((arg current-prefix-arg))
		 (list
		  (if arg
		      (mhtml-ext-read-tag)
		    sgml-tag-last))))
  (let ((tag (or tag sgml-tag-last)))
    (insert (format "</%s>" tag))))

(defsubst mhtml-ext--position-to-marker (pos)
  (save-excursion
    (goto-char pos)
    (point-marker)))


;; --- Syntax-Related Functions

;; Tag
(cl-defstruct mhtml-ext-tag
  "An HTML tag."
  (start nil :type marker
	 :documentation "Position of the starting tag--more precisely, the position
of the \"<\" where the tag name is enclosed.")
  (end nil :type marker
       :documentation "Position of the end tag--more precisely, the position of the
\">\" after the name of the closing tag.")
  (name nil :type string :documentation "The name of the tag.")
  (end-p nil :type bool
	 :documentation "This is t if tag is an end tag, nil otherwise.")
  (attributes nil :type list
	      :documentation "This slot is the list of attributes of a tag.
Each element is is a string that represents a tag attribute.")
  node)

(defun mhtml-ext-tag--node-attrs (node)
  "Return a list of NODE's attributes.
Each element of the returned list is the textual
representation of an attribute."
  (declare (side-effect-free t))
  (tree-sitter-ext-assert-valid-state)
  (tree-sitter-ext-assert-node-type node start_tag end_tag)
  (let* ((q (tsc-make-query tree-sitter-language
			    [(attribute) @attr]))
	 (cs (tsc-query-captures
	      q node #'tsc--buffer-substring-no-properties)))
    (when cs
      (cl-loop for cap across cs
	       collect
	       (let ((attr-node (cdr cap)))
		 (tsc-node-text attr-node))))))

(defun mhtml-ext-tag-string (cl-x)
  (declare (side-effect-free t))
  (let ((attrs (-some->> (mhtml-ext-tag-attributes cl-x)
		 (s-join " "))))
    (concat "<"
	    (and (mhtml-ext-tag-end-p cl-x) "/")
	    (mhtml-ext-tag-name cl-x)
	    (and attrs (concat " " attrs))
	    ">")))

(defun mhtml-ext-tags-from-element-node (node)
  "Return a cons of the start and end tags of NODE.
NODE must be an `element' node."
  (declare (side-effect-free t))
  (tree-sitter-ext-assert-valid-state)
  (tree-sitter-ext-assert-node-type node element)
  (if-let ((q (tsc-make-query
	       tree-sitter-language
	       [((start_tag
		  (tag_name) @start.name)
		 @start
		 (end_tag
		  (tag_name) @end.name)
		 @end)]))
	   (cs (-some-->
		   (tsc-query-captures
		    q node #'tsc--buffer-substring-no-properties
		    mhtml-ext-tree-sitter--query-cursor)
		 (append it nil))))
      (cl-ext-progn
	(cl-assert (>= (length cs) 2) t)
	;; cs is a list of cons cells of the form (TAG . NODE).
	;; TAG will be one of `start.name', `start', `end.name', or `end'.
	;; For each "group" in the list, the order is `start', `start.name',
	;; `end', and `end.name'.
	(when-let ((node1 (cdr (nth 0 cs)))	  ; `start'
		   (nmnode1 (cdr (nth 1 cs))) ; `start.name'
		   (temp (--find-last-index (equal (car it) 'end) cs))
		   (node2 (cl-ext-progn
			    (cl-assert temp)
			    (cdr (nth temp cs))))	  ; `end'
		   (nmnode2 (cdr (nth (1+ temp) cs))) ; `end.name'
		   )
	  (cons (mhtml-ext--tag-from-nodes nmnode1 node1)
		(mhtml-ext--tag-from-nodes nmnode2 node2))))))

(defun mhtml-ext--tag-from-nodes (name-node tag-node)
  "Return a tag that represents NAME-NODE and TAG-NODE."
  (tree-sitter-ext-assert-valid-state)
  (tree-sitter-ext-assert-node-type
   tag-node start_tag end_tag)
  (tree-sitter-ext-with-region tag-node
      ((pm (lambda (pos)
	     (save-excursion
	       (goto-char pos)
	       (point-marker)))))
    (make-mhtml-ext-tag
     :start (funcall pm beg)
     :end (funcall pm end)
     :name (tsc-node-text name-node)
     :end-p (equal (tsc-node-type tag-node) 'end_tag)
     :attributes (mhtml-ext-tag--node-attrs tag-node))))

;; Element
(cl-defstruct mhtml-ext-element
  "An HTML element with a start"
  (start-tag nil :type mhtml-ext-tag)
  (start nil :type marker)
  (end-tag nil :type mhtml-ext-tag)
  (end nil :type marker)
  node)

(defun mhtml-ext-in-element-p ()
  "Return the element surrounding point.
The return value is a `mhtml-ext-element' structure on
success, nil otherwise."
  (tree-sitter-ext-assert-valid-state)
  (let* ((node (tree-sitter-node-at-pos 'element))
	 (temp (and node (mhtml-ext-tags-from-element-node node)))
	 (start-tag (car temp))
	 (end-tag (cdr temp)))
    ;; TODO: consider self-closing tags (e.g., "<input ... />")?
    (and start-tag end-tag
	 (tree-sitter-ext-with-region node nil
	   (make-mhtml-ext-element
	    :start-tag start-tag
	    :start beg
	    :end-tag end-tag
	    :end end
	    :node node)))))

(defun mhtml-ext-rename-element ()
  "Change the name of the containing element."
  (interactive)
  (unwind-protect
      (if-let ((elt (mhtml-ext-in-element-p))
	       (tn (let ((default
			   (mhtml-ext-tag-name
			    (mhtml-ext-element-start-tag elt))))
		     (read-string (format "New Tag (%s): " default)
				  nil nil default))))
	  (let* ((st (mhtml-ext-element-start-tag elt))
		 (et (mhtml-ext-element-end-tag elt))
		 (nst (copy-mhtml-ext-tag st))
		 (net (copy-mhtml-ext-tag et)))
	    (setf (mhtml-ext-tag-name nst) tn
		  (mhtml-ext-tag-name net) tn)
	    (lsp-with-undo-amalgamate
	      (save-excursion
		(delete-region (mhtml-ext-tag-start st)
			       (mhtml-ext-tag-end st))
		(goto-char (mhtml-ext-tag-start st))
		(insert (mhtml-ext-tag-string nst))
		(goto-char (mhtml-ext-tag-start st))
		(delete-region (mhtml-ext-tag-start et)
			       (mhtml-ext-tag-end et))
		(goto-char (mhtml-ext-tag-start et))
		(insert (mhtml-ext-tag-string net)))))
	(user-error "Not inside or at an element"))
    (highlight-ext-remove-all-highlights)))

(defun mhtml-ext-mark-element ()
  "Mark the containing element."
  (interactive)
  (when-let ((elt (mhtml-ext-in-element-p)))
    (or (region-active-p)
	(activate-mark))
    (goto-char (mhtml-ext-element-start elt))
    (push-mark (mhtml-ext-element-end elt))))


(defun mhtml-ext-up-node ()
  ;; TODO: documentation string
  (interactive)
  (tree-sitter-ext-assert-valid-state)
  (mhtml-ext-motion-mode 1)
  (cl-loop named quit
	   with cur-node = mhtml-ext-motion--node
	   with next-node = (tsc-get-parent cur-node)
	   initially do
	   (unless cur-node
	     (mhtml-ext-motion-mode 0)
	     (user-error "Already at root node"))
	   while (equal (tsc-node-type cur-node) 'start_tag)
	   do
	   (setq cur-node next-node
		 next-node (tsc-get-parent next-node))
	   (--message "Current node: %S (%d), next node: %S (%d)"
		      :level 1
		      (tsc-node-type cur-node)
		      (tsc-node-start-position cur-node)
		      (tsc-node-type next-node)
		      (tsc-node-start-position next-node))
	   finally do
	   (goto-char (tsc-node-start-position next-node))
	   (setq mhtml-ext-motion--node next-node)
	   (set-transient-map mhtml-ext-motion--transient-map
			      nil
			      (lambda ()
				(mhtml-ext-motion-mode 0)))))


;; --- Hide/Show Commands

(defun mhtml-ext-hide-element ()
  (interactive)
  (hs-minor-mode 1)
  (tree-sitter-ext-assert-valid-state)
  (if-let ((elt (mhtml-ext-in-element-p)))
      (let ((beg (->> (mhtml-ext-element-start-tag elt)
		      (mhtml-ext-tag-end)))
	    (end (->> (mhtml-ext-element-end-tag elt)
		      (mhtml-ext-tag-start))))
	(hs-ext-hide-range beg end 'code))
    (user-error "Not inside an element")))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'mhtml-ext-hide-element))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t)
   (call-interactively #'menu-bar--toggle-truncate-long-lines))
 t)


;; --- Motion Minor Mode

(defconst mhtml-ext-motion--transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "^") #'mhtml-ext-up-node)
    map))

(defvar-local mhtml-ext-motion--node nil)

(define-minor-mode mhtml-ext-motion-mode
  "Minor mode for motion commands."
  :lighter " HTML Motion"
  :after-hook (mhtml-ext-motion--setup))

(defun mhtml-ext-motion--setup ()
  (condition-case err
      (cl-ext-progn
	(tree-sitter-ext-assert-valid-state)
	(if mhtml-ext-motion-mode
	    (cl-ext-progn
	      (setq mhtml-ext-motion--node
		    (tree-sitter-node-at-pos :named)))
	  (kill-local-variable 'mhtml-ext-motion--node)))
    (error (message "Error setting up motion mode: %S" err)
	   (run-with-idle-timer
	    0.1 nil #'mhtml-ext-motion-mode 0))))


;; ### Keymaps

(define-key mhtml-mode-map (kbd "C-M-i") #'completion-at-point)

(define-prefix-command 'user-ext-mhtml-skeleton-map)
(define-key mhtml-mode-map (kbd "C-c i") #'user-ext-mhtml-skeleton-map)
(define-key user-ext-mhtml-skeleton-map (kbd "e") #'mhtml-ext-insert-entity)
(define-key user-ext-mhtml-skeleton-map (kbd "t") #'mhtml-ext-insert-table)
(define-key user-ext-mhtml-skeleton-map (kbd "r") #'mhtml-ext-insert-table-row)
(define-key user-ext-mhtml-skeleton-map (kbd "o") #'mhtml-ext-insert-tag)
(define-key user-ext-mhtml-skeleton-map (kbd ">") #'mhtml-ext-insert-end-tag)

;; Motion commands
(define-key mhtml-mode-map (kbd "C-c ^") #'mhtml-ext-up-node)

(eval-and-compile
  (define-prefix-command 'user-ext-mhtml-mark-map)
  (define-key mhtml-mode-map (kbd "C-c SPC") #'user-ext-mhtml-mark-map)
  (define-key user-ext-mhtml-mark-map (kbd "e") #'mhtml-ext-mark-element))

;; Hs commands
(eval-and-compile
  (define-prefix-command 'user-ext-mhtml-hs-map)
  (define-key mhtml-mode-map (kbd "C-c f") #'user-ext-mhtml-hs-map)
  (define-key user-ext-mhtml-hs-map (kbd "e") #'mhtml-ext-hide-element))


;; --- Menu

(easy-menu-define user-ext-mhtml-menu-map
  mhtml-mode-map
  "HTML"
  '("HTML Extension"
    ["Rename Element" mhtml-ext-rename-element
     :active mhtml-ext-tree-sitter-mode]
    ["Mark Element" mhtml-ext-mark-element
     :active mhtml-ext-tree-sitter-mode]))


;; ### Extension Hook

;;;###autoload
(defun mhtml--extra-hook ()
  (setq-local tempo-user-elements
	      (cons #'tempo-ext-tempo-handler
		    tempo-user-elements))
  (mhtml-ext-tree-sitter-mode 1))

;;;###autoload
(add-hook 'mhtml-mode-hook #'mhtml--extra-hook)


(extension-provide 'html-ext)
;;; html-ext ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "mx" "mhtml-ext")
;; eval: (abbrev-ext-define-local-abbrev "mts" "mhtml-ext-tree-sitter")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-mhtml")
;; eval: (abbrev-ext-define-local-abbrev "tse" "tree-sitter-ext")
;; eval: (abbrev-ext-define-local-abbrev "ts" "tree-sitter")
;; eval: (abbrev-ext-define-local-abbrev "tx" "tempo-ext")
;; End:
