;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")
(extension-check-requires plantuml-mode)

(require 'dash)
(require 'plantuml-mode)

(eval-when-compile
  (require 'thunk)
  (require 'llama)
  (require 'alist-ext)
  (require 'debug-ext))


;; ### Customization

(defgroup plantuml-ext nil
  "PlantUML mode extension."
  :group 'user-extensions)

(defface user-ext-plantuml-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for variables.")


;; ### Variables

(defmacro plantuml-ext-rx (&rest regexps)
  "Variation of `rx' with support for PlantUML constructs.

lolipop-left      Left-facing Lolipops (e.g., \"o-\")
lolipop-right     Right-facing lolipops (e.g., \"-o\")
composition-left  Left-facing composition arrows (e.g., \"*-\")
composition-right Right-facing composition arrows (e.g., \"-*\")
identifier-char   Any valid identifier (i.e., word or symbol character)"
  (declare (indent 1))
  `(rx-let ((lolipop-left (seq "()-" (opt ?-)))
	    (lolipop-right (seq (opt ?-) "-()"))
	    (composition-left (seq "*-" (opt ?-)))
	    (composition-right (seq (opt ?-) "-*"))
	    (identifier-char (or (syntax word) (syntax symbol)))
	    (pre-processor-start (seq bol (* (syntax whitespace)) ?!)))
     (rx ,@regexps)))

(defconst user-ext-plantuml-variable-face
  'user-ext-plantuml-variable-face
  "Variable alias for face `user-ext-plantuml-variable-face'.")

(defconst user-ext-plantuml-variable-declaration-regexp
  (plantuml-ext-rx
      pre-processor-start (group ?$ (+ identifier-char))
      (* (syntax whitespace)) (opt ??) ?= (* (syntax whitespace))
      (+ nonl))
  "Regular expression for matching variable declarations.
Group 1 matches the name of the variable.")

(defconst user-ext-plantuml-variable-regexp
  (plantuml-ext-rx
      word-start ?$ (+ identifier-char) word-end)
  "Regular expression for matching variables.")

(defconst user-ext-plantuml-indent-else-regexp
  (plantuml-ext-rx bol "else" (* nonl) eol)
  "Regular expression for the start of else blocks.")

(defconst user-ext-plantuml-font-lock-global-keywords
  `((plantuml-ext--font-lock-search-variables . user-ext-plantuml-variable-face)
    (,user-ext-plantuml-variable-declaration-regexp . (1 user-ext-plantuml-variable-face)))
  "Font lock keywords.")

(defvar user-ext-plantuml-submodes nil
  "A list of currently-defined so-called submodes.")
(put 'user-ext-plantuml-submodes 'risky-local-variable t)

(defvar user-ext-plantuml-submodes--short nil
  "A list of currently-defined so-called submodes (short names).")
(put 'user-ext-plantuml-submodes--short 'risky-local-variable t)

(defvar-local user-ext-plantuml-current-submode nil
  "The currently active submode.")

(defvar-local user-ext-plantuml-submode-indent-regexp-start nil)

(defvar-local user-ext-plantuml-submode-indent-regexp-end nil)

(defvar-local user-ext-plantuml-submode-indent-inline-regexp nil)

(defvar-local plantuml-ext-mode--active-timer nil)

(defvar-local user-ext-plantuml--variables nil
  "Local list of variables.")

(defvar-local user-ext-plantuml--variables-regexp nil
  "Regular expression for the variables in `user-ext-plantuml--variables'.")


;; ### Functions

(defun plantuml-ext-debug-fix-submodes-list ()
  "Meant for internal use.
Fix `user-ext-plantuml-submodes' whenever it contains
elements that should be in `user-ext-plantuml-submodes--short'."
  (interactive)
  (setq user-ext-plantuml-submodes (seq-difference
				    user-ext-plantuml-submodes
				    (cons 'uml user-ext-plantuml-submodes--short))))

(fext-defadvice plantuml-current-block-depth
    (override plantuml-current-block-depth nil)
  "Trace the current block indentation level by recursively looking back line by line.

This is not the original function; it is the replacement."
  (save-excursion
    (let ((relative-depth 0))
      (beginning-of-line)
      (when (and (not (looking-at user-ext-plantuml-submode-indent-inline-regexp))
		 (-any? #'looking-at user-ext-plantuml-submode-indent-regexp-end))
	(cl-decf relative-depth))
      (while (not (bobp))
        (forward-line -1)
	(cl-ext-cond
	 ((looking-at user-ext-plantuml-submode-indent-inline-regexp)
	  t)
	 ((looking-at-p user-ext-plantuml-indent-else-regexp)
	  t)
	 ((-any? #'looking-at-p user-ext-plantuml-submode-indent-regexp-end)
	  (cl-decf relative-depth))
	 ((-any? #'looking-at-p user-ext-plantuml-submode-indent-regexp-start)
	  (cl-incf relative-depth))))
      ;; (if (<= relative-depth 0)
      ;;     0
      ;;   relative-depth)
      (max relative-depth 0))))

(defmacro plantuml-ext--rx-case (&rest args)
  "Undocumented.

\(fn ((ARROW-RX COND-STRING REPL1 REPL2)...))"
  (declare (indent 0))
  (let (clauses)
    (setq clauses
	  (cl-loop
	   while args
	   collect
	   (cl-destructuring-bind (arrow-rx cond-string repl1 repl2) (pop args)
	     `((looking-at ,arrow-rx)
	       (let ((arrow (match-string 1)))
		 (if (string= arrow ,cond-string)
		     (replace-match ,repl1 nil nil nil 1)
		   (replace-match ,repl2 nil nil nil 1)))))))
    `(cond ,@clauses)))

;; TODO: Docstring
(defun plantuml-ext-flip-arrow-direction ()
  "docstring"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (plantuml-ext--rx-case
      ("\\*--?" "*--" "--*" "-*")
      ("--\\*" "--*" "*--" "*-"))))

(defmacro plantuml-ext-read-only (&rest body)
  "Evaluate BODY in read-only mode."
  (declare (debug (body)) (indent defun))
  `(let ((read-only-status buffer-read-only)
	 (buffer-read-only t))
     ,@body))


;; --- Font Lock

(defun plantuml-ext--font-lock-search-variables (limit)
  (if (and user-ext-plantuml--variables-regexp
	   (re-search-forward user-ext-plantuml--variables-regexp limit t))
      (let ((beg (match-beginning 0))
	    (end (match-end 0)))
	(list beg end))))


;; ### Submodes

(cl-defmacro plantuml-ext-define-submode (name mappings &key on-body off-body)
  "Define a submode NAME with its keymap set from MAPPINGS.
A keymap will be generated from MAPPINGS and placed in
user-ext-plantuml-NAME-submode-map.  A function will be
generated called plantuml-ext-NAME-submode.

When the submode is enabled, the other submodes are disabled,
and the :on-body forms are run.  When the submode is
disabled, the :off-body forms are run.

MAPPINGS is a list whose elements take the form
(KEY . DEFINITION).  Each key is wrapped in `kdb'.

\(fn NAME MAPPINGS [:on-body (FORM...)] [:off-body (FORM...)] [:variables ((NAME VALUE)...)])"
  (declare (indent 1) (debug (&define name
				      ([&rest (stringp . function-form)])
				      [&optional ":on-body" def-body]
				      [&optional ":off-body" def-body])))
  (let ((mode-symbol (intern (format "plantuml-ext-%S-mode" name)))
	(mode-map-name (intern (format "plantuml-ext-%S-mode-map" name)))
	(string-name (symbol-name name))
	(map (make-sparse-keymap)))
    (cl-check-type name symbol)
    (cl-check-type mappings alist)
    (cl-check-type on-body (or list null))
    (cl-check-type off-body (or list null))
    (alist-ext-dolist (key-seq def mappings)
      (define-key map (kbd key-seq) def))
    `(prog1 ',mode-symbol
       (cl-pushnew ',mode-symbol user-ext-plantuml-submodes)
       (cl-pushnew ',name user-ext-plantuml-submodes--short)
       (defvar-local ,mode-map-name ',map)
       (define-minor-mode ,mode-symbol
	 ,(format "Submode for %s diagrams.

\\{%S}" string-name mode-map-name)
	 :keymap ',mode-map-name
	 :lighter ,(format " PlantUML[%S]" name)
	 (if ,mode-symbol
	     (cl-ext-progn
	       (setq user-ext-plantuml-current-submode ',mode-symbol)
	       (unless plantuml-ext-mode--active-timer
		 (run-with-idle-timer
		  0.1 nil
		  (lambda ()
		    (let ((x (seq-difference user-ext-plantuml-submodes
					     (quote ,(list mode-symbol))
					     #'eq)))
		      (setq plantuml-ext-mode--active-timer t)
		      (dolist (mode x nil)
			(funcall mode 0))
		      (setq plantuml-ext-mode--active-timer nil)))))
	       (setq user-ext-plantuml-submode-indent-inline-regexp
		     "^bus;\":,,b.g,asad56237"
		     user-ext-plantuml-submode-indent-regexp-end
		     plantuml-indent-regexp-end
		     user-ext-plantuml-submode-indent-regexp-start
		     plantuml-indent-regexp-start)
	       ,@on-body)
	   ,@off-body))
       (setq-default ,mode-map-name ',map))))

(defmacro plantuml-ext-define-submode-function (name submode arglist docstring &rest body)
  "Define a submode function NAME for SUBMODE.
The function is called plantuml-ext-NAME-SUBMODE.  ARGLIST,
DOCSTRING, and BODY are treated the same as in `defun',
which see.

An error is raised if SUBMODE is not a valid submode, i.e.,
defined via `plantuml-ext-define-submode'."
  (declare (indent 3) (doc-string 3)
	   (debug (&define name symbolp (&rest arg) stringp
			   [&optional ("interactive" interactive)]
			   def-body)))
  (cl-check-type name symbol)
  (cl-check-type submode symbol)
  (cl-check-type arglist list)
  (cl-check-type docstring string)
  (unless (memq submode user-ext-plantuml-submodes--short)
    (error "%S is not a submode" submode))
  (let ((fname (intern (format "plantuml-ext-%S-%S" name submode)))
	(docstring (format "%s\n\nThis function is defined for %s submode."
			   docstring submode))
	(mode-name (intern (format "plantuml-ext-%S-mode" submode)))
	int-form)
    (pcase (car body)
      (`(interactive . ,_x)
       (setq int-form (pop body))))
    `(progn
       (defun ,fname ,arglist
	 ,docstring
	 ,int-form
	 (unless (eq user-ext-plantuml-current-submode ',mode-name)
	   (user-error ,(concat (format "This can only be called in %S submode"
					submode))))
	 ,@body))))

(defmacro plantuml-ext-define-skeleton (name submode docstring &rest skel)
  "Define skeleton NAME for SUBMODE.
DOCSTRING is added as the command's docstring, and SKEL is
the actual skeleton.  The command's name is
plantuml-ext-skeleton-NAME-SUBMODE.

The skeleton command is assigned :before advice via
`fext-defadvice' which checks the current submode of the
buffer."
  (declare (debug (&define name [&or symbolp (&rest symbolp)]
			   stringp skeleton-edebug-spec))
	   (indent 2) (doc-string 3))
  (cl-check-type name symbol)
  (cl-check-type docstring string)
  (let (symbol mode-name advice)
    ;; TODO: Document changes around SUBMODE
    (cl-typecase submode
      (symbol
       (setq symbol (intern (format "plantuml-ext-skeleton-%S-%S" name submode))
	     mode-name (intern (format "plantuml-ext-%S-mode" submode))
	     docstring (format "%s\n\nThis function was defined for %s submode."
			       docstring submode)
	     advice
	     `(fext-defadvice ,symbol (before ,symbol)
		(unless (eq user-ext-plantuml-current-submode ',mode-name)
		    (user-error ,(format "This can only be called in %S submode"
					 submode)))))
       (unless (memq submode user-ext-plantuml-submodes--short)
	   (error "Invalid submode %s" submode)))
      (list
       (cl-loop with submodes = submode
		with mode-symbols
		for submode in submodes
		do
		(unless (cl-typep submode 'symbol)
		    (signal-type-error submode (type-of submode) 'list))
		(unless (memq submode user-ext-plantuml-submodes--short)
		    (error "Invalid submode %s" submode))
		(cl-pushnew (intern (format "plantuml-ext-%S-mode" submode))
			    mode-symbols)
		finally do
		(setq symbol (intern (format "plantuml-ext-skeleton-%S" name))
		      docstring
		      (format "%s\n\nThis function was defined for these submodes:\n%s"
			      docstring (mapconcat (lambda (x) (format "- %s" x))
						   submodes "\n"))
		      advice
		      `(fext-defadvice ,symbol (before ,symbol)
			 (unless (memq user-ext-plantuml-current-submode ',mode-symbols)
			     (user-error ,(concat "This can be called in submodes "
						  (mapconcat #'symbol-name submodes ", "))))))))
      (t (signal 'wrong-type-argument
		 (list '(or symbol list) submode 'submode))))
    `(prog1 ',symbol
       (define-skeleton ,symbol ,docstring ,@skel)
       (fext-defadvice ,symbol (before ,symbol) :remove)
       ,advice)))


;; --- Json

(plantuml-ext-define-submode json
  (("C-c C-s j" . plantuml-ext-skeleton-json-json)))

(plantuml-ext-define-skeleton json json
  "Insert a JSON graph template."
  nil
  "@startjson" \n
 "{" \n _ \n "}" > \n
  "@endjson")


;; --- Salt

(plantuml-ext-define-submode salt
  (("C-c C-s s" . plantuml-ext-skeleton-salt-salt)
   ("C-c C-s t" . plantuml-ext-insert-table-salt)
   ("C-c C-s [" . plantuml-ext-skeleton-button-salt))
  :on-body
  ((let* ((table-start
	   (plantuml-ext-rx
	       ?\{ (* (or (any ?S ?I ?T ?# ?+ ?- ?! ?/)
			  (seq ?^ ?\" (* nonl) ?\")))))
	  (table-end (plantuml-ext-rx ?\}))
	  (inline-table-regexp
	   (plantuml-ext-rx bol (* (syntax whitespace))
			    (regexp table-start)
			    (* nonl) (regexp table-end) eol)))
     (setq user-ext-plantuml-submode-indent-regexp-start
	   (list (plantuml-ext-rx bol (* (syntax whitespace))
				  (regexp table-start) eol))
	   user-ext-plantuml-submode-indent-regexp-end
	   (list (plantuml-ext-rx bol (* (syntax whitespace))
				  (regexp table-end) (* nonl) eol))
	   user-ext-plantuml-submode-indent-inline-regexp
	   inline-table-regexp))))

(plantuml-ext-define-skeleton salt salt
  "Insert salt (wireframe) template."
  nil
  "@startsalt" \n
  _ \n
  "@endsalt")

(plantuml-ext-define-skeleton button salt
  "Insert a button."
  "Text: "
  ?\[ str ?\])

(plantuml-ext-define-submode-function insert-table salt (modifier arg)
  "Insert a table."
  (interactive (list (read-char (eval-when-compile
				  (format "Prompt (%s): "
					  (mapconcat (##char-to-string %1)
						     '(?\  ?S ?^ ?T)
						     ", "))))
		     current-prefix-arg))
  (let ((linemod (thunk-delay
		  (char-to-string
		   (read-char (eval-when-compile
				(format "Space modifier (%s): "
					(mapconcat (##char-to-string %1)
						   '(?+ ?- ?# ?! ?\ )
						   ", "))))))))
    (pcase modifier
      (32				; " "
       ;; "normal" table
       (setq modifier (thunk-force linemod)))
      (?^
       (setq modifier (format "%c\"%s\"" modifier
			      (read-string "Group name: " nil t))))
      (?S
       (let ((orient (read-char-choice "Orientation (- [horizontal], I [vertical]): "
				       '(?- ?I) t)))
	 (setq modifier (format "%c%c" modifier orient))))
      (?T (setq modifier (concat "T" (thunk-force linemod))))
      (_ (user-error "Invalid modifier \"%c\"" modifier)))
    (if arg
	(skeleton-insert '(nil ?\{ str _  ?\}) nil modifier)
      (skeleton-insert '(nil ?\{ str \n _ \n ?\} >) nil modifier))))


;; --- Uml Base

(plantuml-ext-define-skeleton uml
    (uml/activity uml/class)
  "Insert PlantUML template."
  nil
  "@startuml" \n
  _ \n
  "@enduml")


;; --- Uml/Activity

(plantuml-ext-define-submode uml/activity
  (("C-c C-s u" . plantuml-ext-skeleton-uml)
   ("C-c C-s n" . plantuml-ext-skeleton-note-uml/activity)
   ("C-c C-s N" . plantuml-ext-skeleton-ml-note-uml/activity))
  :on-body ((add-hook 'post-self-insert-hook
		      #'plantuml-ext-post-self-insert-uml/activity nil t)
	    (push (plantuml-ext-rx
		      bol "else" (+ nonl))
		  user-ext-plantuml-submode-indent-regexp-start)
	    user-ext-plantuml-submode-indent-regexp-end)
  :off-body ((remove-hook 'post-self-insert-hook
			  #'plantuml-ext-post-self-insert-uml/activity t)))

(plantuml-ext-define-skeleton ml-note uml/activity
  "Insert a multiline note."
  "Direction (left/right): "
  "note " str \n
  _ \n
  "end note" >)

(plantuml-ext-define-skeleton note uml/activity
  "Insert a single-line note."
  "Direction (left/right): "
  "note " str ": " _ >)

(plantuml-ext-define-submode-function post-self-insert uml/activity ()
  "Post self-insert hook."
  (interactive)
  (let ((event last-command-event)
	(boi (= (current-column) (1+ (current-indentation)))))
    (cl-case event
      (?: (cl-ext-when boi
	      (skeleton-insert
	       '(nil _ ?\;))))
      (?| (cl-ext-when boi
	      (skeleton-insert
	       '(nil _ ?|)))))))


;; --- Uml/Class

(plantuml-ext-define-submode uml/class
  (("C-c C-s u" . plantuml-ext-skeleton-uml)
   ("C-c C-s n" . plantuml-ext-insert-note-uml/class)
   ("C-c C-s p" . plantuml-ext-skeleton-package-uml/class)
   ("C-c C-s M-p" . plantuml-ext-skeleton-protocol-uml/class)
   ("C-c C-s c" . plantuml-ext-skeleton-class-uml/class)))

(plantuml-ext-define-skeleton package uml/class
  "Insert a package block."
  "Name: "
  "package " str " {" \n
  _ \n
  ?\} >)

(plantuml-ext-define-skeleton class uml/class
  "Insert a class."
  "Name: "
  "class " str
  (progn (setq v1 (read-string "Stereotype: " nil t))
	 (if (string-empty-p v1)
	     " {"
	   (format " << %s >> {" v1)))
  \n _ \n
  ?\} >)

(plantuml-ext-define-skeleton protocol uml/class
  "Insert a protocol."
  "Name: "
  "protocol " str
  (progn (setq v1 (read-string "Stereotype: " nil t))
	 (if (string-empty-p v1)
	     " {"
	   (format " << %s >> {" v1)))
  \n _ \n
  ?\} >)

;; TODO: Test this function
(plantuml-ext-define-submode-function insert-note
    uml/class ()
  "Insert a class diagram inline note."
  (interactive)
  (let ((name (read-string "Name (optional): " nil t)))
    (if (string-empty-p name)
	(skeleton-insert '(nil "note " (plantuml-ext--alignment) " : "))
      (skeleton-insert '(nil "note \"" _ "\" as " str)
		       nil name))))


;; --- Wbs

(plantuml-ext-define-submode wbs
  (("C-c C-s w" . plantuml-ext-skeleton-wbs-wbs)))

(plantuml-ext-define-skeleton wbs wbs
  "Insert a WBS graph template."
  nil
  "@startwbs" \n
  _ \n
  "@endwbs")


;; ### Functions

(defun plantuml-ext-is-inside-function (&optional pos))

(defun plantuml-ext--get-global-variables ()
  (save-excursion
    (goto-char (point-min))
    (setq user-ext-plantuml--variables
	  (->> (cl-loop while (re-search-forward user-ext-plantuml-variable-declaration-regexp
						 nil t)
			collect (cl-ext-progn
				  (match-string-no-properties 1)))
	       (-remove #'null)))
    (cl-assert (plantuml-ext-safe-list user-ext-plantuml--variables 'string) t)
    (setq user-ext-plantuml--variables-regexp
	  (plantuml-ext-rx word-start (regexp (regexp-opt user-ext-plantuml--variables))
			   word-end))))
(--ignore
 (cl-prettyprint (symbol-function 'plantuml-ext--get-global-variables))
 t)

(defun plantuml-ext-safe-list (SEQ &optional type)
  "Return non-nil if SEQ is a safe list."
  (cl-loop with safe = t
	   with istype = (if type
			     (##cl-typep %1 type)
			   (lambda (_x) t))
	   for elt in SEQ
	   do
	   (cl-ext-unless (and elt (funcall istype elt))
	       (setq safe nil))
	   finally return
	   safe))
(--ignore
 (cl-prettyprint (symbol-function 'plantuml-ext-safe-list))
 (plantuml-ext-safe-list '("1" "nil") 'string)
 (plantuml-ext-safe-list '("1" "nil" 2) 'string)
 t)

(defun plantuml-ext--alignment ()
  "Query the user for the alignment of an object.
Return a string denoting a direction (left, right, top, or
bottom) followed by a target (if provided)."
  (let* ((directions '("left" "right" "top" "bottom"))
	 (direction (completing-read "Direction: " directions))
	 (target (if (cl-member direction directions :test #'string=)
		     (read-string (format "%s of? (leave empty to omit) " direction))
		   (user-error "Empty or invalid direction %S" direction))))
    (if (string-empty-p target)
	direction
      (format "%s of %s" direction target))))


;; ### Setup

(defun plantuml-ext--after-save ()
  (plantuml-ext-read-only
    (plantuml-ext--get-global-variables)))

;;;###autoload
(defun plantuml--extra-hook ()
  (setq indent-tabs-mode nil tab-width 4)
  (font-lock-add-keywords nil user-ext-plantuml-font-lock-global-keywords)
  (add-hook 'after-save-hook #'plantuml-ext--after-save nil t)
  (run-with-idle-timer 1 nil (lambda ()
			       (plantuml-ext--get-global-variables)))
  (plantuml-ext-uml/class-mode))


;; --- Keymap

(easy-menu-define user-ext-plantuml-menu-map plantuml-mode-map
  "PlantUML menu."
  '("PlantUML"
    ("Submodes"
     ["JSON" plantuml-ext-json-mode
      :selected plantuml-ext-json-mode
      :style radio]
     ["Salt" plantuml-ext-salt-mode
      :selected plantuml-ext-salt-mode
      :style radio]
     ["UML/Activity" plantuml-ext-uml/activity-mode
      :selected plantuml-ext-uml/activity-mode
      :style radio]
     ["UML/Class" plantuml-ext-uml/class-mode
      :selected plantuml-ext-uml/class-mode
      :style radio]
     ["WBS" plantuml-ext-wbs-mode
      :selected plantuml-ext-wbs-mode
      :style radio])))

(keymaps-ext-set-keymap plantuml-mode-map "C-M-i" #'completion-at-point)

;;;###autoload
(add-hook 'plantuml-mode-hook #'plantuml--extra-hook)

(extension-provide 'plantuml-ext)
;;; plantuml-ext.el ends here

;; Local Variables:
;; eval: (progn (cl-ext-unless (fboundp 'mc-submode-skeleton-mapping) (defalias 'mc-submode-skeleton-mapping #[(&optional arg) "\301\302\"\207" [arg kmacro-exec-ring-item ([40 34 67 45 99 32 67 45 115 32 21 24 81 75 101 121 58 32 13 13 right 32 46 32 112 108 97 110 116 117 109 108 45 101 120 116 45 115 107 101 108 101 116 111 110 45 21 24 81 83 107 101 108 101 116 111 110 32 127 58 32 13 13 45 21 24 81 83 117 98 109 111 100 101 58 32 13 13] 0 "%d")] 3 nil "p"] "Keyboard macro.")))
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "plx" "plantuml-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-plantuml")
;; End:
