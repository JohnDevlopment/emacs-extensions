;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")

(require 'cl-lib)
(require 'comint)
(require 'dash)
(require 'go-mode)
(require 'hideshow)
(require 'tempo)
(require 'tree-sitter)
(require 'compat-28)

(use-package company-capf
  :defer t
  :commands
  company-capf)

(use-package tempo-ext
  :autoload
  tempo-ext-on-region
  tempo-ext--handle-if-condition
  tempo-ext-tempo-handler)

(eval-when-compile
  (require 'llama)
  (require 'cl-ext)
  (require 'generator)
  (require 'debug-ext)
  (require 'easymenu))

(cl-declaim (optimize (safety 2)))

(eval-and-compile
  (embed-doc-document-symbol go-ext
    "Go extension."
    :commands
    go-ext-add-dependency
    go-ext-autotype-file
    go-ext-buffer-types
    go-ext-builtin-types
    go-ext-docedit
    go-ext-docedit-apply
    go-ext-docedit-save
    go-ext-godoc
    go-ext-godoc-server
    go-ext-godoc-server-stop
    go-ext-goto-function-or-method
    go-ext-goto-package
    go-ext-goto-struct
    go-ext-hide-function
    go-ext-hide-method
    go-ext-hide-struct
    go-ext-menu--goto-function
    go-ext-menu--goto-function-name
    go-ext-process-kill-buffer
    go-ext-process-no-edit
    go-ext-project-rename-buffer
    :functions
    go-ext-debug-body
    go-ext-define-auxillery-skeleton
    go-ext-define-hide-all-x-function
    go-ext-define-skeleton
    go-ext-erase-buffers
    go-ext-go-process-run-command
    go-ext-in-field-p
    go-ext-in-function-p
    go-ext-in-lambda-p
    go-ext-in-method-p
    go-ext-in-struct-p
    go-ext-in-type-p
    go-ext-kill-buffer
    go-ext-kill-buffer-on-error
    go-ext-make-node-from-x-function
    go-ext-pos-at-line-col
    go-ext-project-current
    go-ext-project-package
    go-ext-project-parent
    go-ext-read-type
    go-ext-rx
    go-ext-skeleton-no-newline
    go-ext-tempo-define-template
    go-ext-tree-sitter-body-region
    go-ext-try-project
    :customs
    user-ext-go-godoc-default-args
    user-ext-go-indent-tabs
    user-ext-go-tab-width
    :variables
    user-ext-go-godoc-args-history
    user-ext-go-hide-map
    user-ext-go-package-history
    user-ext-go-read-type-history
    user-ext-go-skeleton-map
    :constants
    go-ext-docedit-buffer
    go-ext-docedit-mode-map
    go-ext-docedit-overlay-category
    go-ext-docedit-register
    go-ext-go-process-mode-map
    user-ext-go--optimize
    user-ext-go-field-re
    user-ext-go-godoc-error-buffer
    user-ext-go-godoc-help-buffer
    user-ext-go-godoc-output-buffer
    user-ext-go-godoc-server-process-buffer
    user-ext-go-process-buffer
    user-ext-go-struct-decl-re))

(extension-check-requires go-mode)


;; ### Customization

(defgroup go-ext nil
  "Go language extension."
  :group 'user-extensions)

(defcustom user-ext-go-indent-tabs nil
  "Controls the local value of `indent-tabs-mode'."
  :type '(choice (const :tag "Tabs" t)
		 (const :tag "Spaces" nil))
  :group 'go-ext)

(defcustom user-ext-go-tab-width 4
  "Tab width in columns."
  :type 'integer
  :group 'go-ext)

(defcustom user-ext-go-custom-types nil
  "A list of types to be returned by `go-ext-custom-types'."
  :type '(repeat string)
  :safe #'list-of-strings-p
  :group 'go-ext)

(defcustom user-ext-go-godoc-default-args
  "-cmd"
  "Default commandline arguments for Godoc."
  :group 'go-ext
  :type 'string)


;; ### Variables

(defvar user-ext-go-subextensions nil "List of loaded subextensions.")

(defconst user-ext-go-process-buffer "*go*"
  "The name of the process buffer for generic Go processes.")

(defvar user-ext-go-package-history nil
  "History for `go-ext--package-complete'.")

(defvar user-ext-go-read-type-history nil)

(defconst user-ext-go--optimize 1)


;; ### Syntax

(load-extension "go-subext_syntax")


;; --- Godoc

(define-obsolete-variable-alias 'user-ext-go-godoc-process-buffer
  'user-ext-go-godoc-output-buffer "2025-09-24")
(defconst user-ext-go-godoc-output-buffer "*godoc output*"
  "The name of the output buffer for godoc.")

(defconst user-ext-go-godoc-error-buffer "*godoc error*"
  "Buffer for errors produced by Godoc.")

(defconst user-ext-go-godoc-help-buffer "*godoc help*"
  "Help buffer for Go commands.")

(defvar user-ext-go-godoc-args-history nil
  "History of Godoc command-line arguments.")
(define-obsolete-variable-alias 'user-ext-go-godoc-args-history
  'user-ext-go-godoc-server-args-history "2025-09-21")

(defconst user-ext-go-godoc-server-process-buffer "*godoc server*"
  "The name of the process buffer for a godoc server.")


;; --- Regular Expressions

(defmacro go-ext-rx (&rest regexps)
  "Translate Go-specific regular expressions REGEXPS to a string.

In addition to the constructs of `rx', the following
constructs are available.

keyword     Match a Go keyword
identifier  Match a valid Go identifier"
  `(rx-let ((keyword (seq word-start
			  (or "break" "default" "func" "interface" "select"
			      "case" "defer" "go" "map" "struct"
			      "chan" "else" "goto" "package" "switch"
			      "const" "fallthrough" "if" "range" "type"
			      "continue" "for" "import" "return" "var")
			  word-end))
	    (identifier (seq word-start (+ (or word digit)) word-end)))
     (rx ,@regexps)))

(defconst user-ext-go-struct-decl-re
  (go-ext-rx bol (* space) "type" (+ space)
	     (group identifier) (+ space) "struct")
  "Regular expression for struct declarations.
Group 1 matches the name of the struct.")

(defconst user-ext-go-field-re
  (go-ext-rx bol (+ space) (group identifier)
	     (+ space) (group identifier))
  "Regular expression for struct fields.
Group 1 matches the name.
Group 2 matches the type.")


;; ### Functions

(defmacro go-ext-debug-body (opt-level &rest body)
  "Do BODY if the optimization level is < OPT-LEVEL.
OPT-LEVEL is compared with `user-ext-go--optimize': if the
latter is less than the former, BODY is evaluated."
  (declare (indent 1) (debug (integerp &rest form)))
  (unless (> opt-level user-ext-go--optimize)
    (cons 'progn body)))

(defsubst go-ext--combine-lists (list1 list2 &optional compare-fn)
  (let* ((-compare-fn (or compare-fn)))
    (-union list1 list2)))

(define-obsolete-function-alias 'go-ext-tree-sitter-assert-node-type
  #'tree-sitter-ext-assert-node-type "2025-10-01")

(defun go-ext-pos-at-line-col (line column)
  "Return the buffer position at LINE and COLUMN."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (move-to-column column)
    (point)))

(defmacro go-ext-tempo-define-template (name doc elements)
  "Define a template NAME which inserts ELEMENTS.
The template will be called tempo-template-go-ext-NAME.
DOCSTRING is the documentation string for the command.
ELEMENTS is a list of elements recognized by
`tempo-define-template', which see.

The Go extension adds `tempo-ext-tempo-handler' to
`tempo-user-elements', so additional elements are available.
\(See the documentation for `tempo-ext-tempo-handler'.)

\(fn NAME DOCSTRING ELEMENTS)"
  (declare (indent defun) (doc-string 2)
	   (debug (&define name stringp sexp)))
  (cl-check-type name string)
  (cl-check-type doc string)
  (cl-check-type elements list)
  (let* ((name (format "go-ext-%s" name))
	 (fname (format "tempo-template-%s" name)))
    `(prog1 ',(intern fname)
       (tempo-define-template ,name ',elements nil ,doc))))

(defmacro go-ext-define-auxillery-skeleton (name nl &rest skel)
  "Define an auxillery skeleton NAME which inserts SKELETON.
The command will be called go-ext-skeleton--NAME.
DOCSTRING will be its documentation.
NEWLINE will be used as the value of `skeleton-end-newline'.

\(fn COMMAND NEWLINE SKELETON...)"
  (declare (debug (&define name booleanp skeleton-edebug-spec))
	   (indent defun))
  (cl-check-type name symbol)
  (let ((fname (intern (format "go-ext-skeleton--%S" name)))
	(doc-name (string-replace "-" " "
				  (symbol-name name))))
    `(prog1 ',fname
       (defun ,fname (&optional str arg)
	 ,(format "Insert a %s.

This is an auxillery skeleton command (see ‘skeleton-insert’).
Normally the skeleton text is inserted at point, with nothing \"inside\".
If there is a highlighted region, the skeleton text is wrapped
around the region text.
%s

A prefix argument ARG says to wrap the skeleton around the next ARG words.
A prefix argument of -1 says to wrap around region, even if not highlighted.
A prefix argument of zero says to wrap around zero words---that is, nothing.
This is a way of overriding the use of a highlighted region."
		  doc-name
		  (if nl "This inserts a newline at the end."
		    "This does not insert a newline at the end."))
	 (interactive "*P\nP")
	 (when (y-or-n-p ,(format "Insert %s? " doc-name))
	   (let ((skeleton-end-newline ,nl))
	     (skeleton-proxy-new ',skel str arg)))))))

(defmacro go-ext-define-skeleton (name doc &rest skel)
  "Define a skeleton NAME which inserts SKELETON.
The command will be called go-ext-skeleton-NAME.
DOCSTRING will be its documentation.

\(fn COMMAND DOCSTRING SKELETON...)"
  (declare (debug (&define name stringp skeleton-edebug-spec))
	   (indent defun) (doc-string 2))
  (cl-check-type name symbol)
  (cl-check-type doc string)
  (let ((fname (intern (format "go-ext-skeleton-%S" name))))
    `(prog1 ',fname
       (define-skeleton ,fname ,doc ,@skel))))

(define-obsolete-function-alias 'go-ext-tempo-on-region
  #'tempo-ext-on-region "2025-09-19")

(define-obsolete-function-alias 'go-ext--tempo-handle-if-condition
  #'tempo-ext--handle-if-condition "2025-09-19")

(define-obsolete-function-alias 'go-ext-tempo-handler
  #'tempo-ext-tempo-handler "2025-09-19")

(defun go-ext-autotype-file ()
  "Auto insert a basic file template."
  (interactive)
  (let ((bn (buffer-name))
	(bfn (buffer-file-name)))
    (cl-ext-cond
      ((and (string-match-p "\\`main\\.go" bn)
	    (y-or-n-p "Is this the main package? "))
       ;; main.go
       (call-interactively #'go-ext-skeleton-package-main))
      ((string-match "_test\\.go\\'" bn)
       (call-interactively #'tempo-template-go-ext-test-file))
      ((and (string-match-p "\\.go\\'" bn)
	    (not (string-match-p "\\`main\\.go" bn)))
       (call-interactively #'tempo-template-go-ext-go-file)))))

(defun go-ext-kill-buffer (buffer-or-name)
  "Kill the buffer specified by BUFFER-OR-NAME.
The argument is the same as for `kill-buffer' (which see),
except that BUFFER-OR-NAME cannot be nil."
  (cl-check-type buffer-or-name (or buffer string))
  (when (or (bufferp buffer-or-name)
	    (get-buffer buffer-or-name))
    (kill-buffer buffer-or-name)))

(defun go-ext-erase-buffers (&rest buffers-or-names)
  "Erase the contents of BUFFERS-OR-NAMES.
Each argument is either a buffer or the name of a buffer.
Erase each buffer listed as argument."
  (declare-function View-exit "view")
  (save-current-buffer
    (dolist (buffer-or-name buffers-or-names)
      (when (or (bufferp buffer-or-name)
		(get-buffer buffer-or-name))
	(set-buffer buffer-or-name)
	(and (bound-and-true-p view-mode)
	     (View-exit))
	(erase-buffer)))))

(defmacro go-ext-kill-buffer-on-error (buffer-name &rest body)
  "Evaluate BODY and kill BUFFER-NAME on error.
If, during the evaluation of BODY, there is an error,
display a message about it and kill BUFFER-NAME.
BUFFER-NAME is a string denoting a name of a buffer."
  (declare (indent 1) (debug ([&or stringp symbolp] body)))
  `(condition-case err
       (progn ,@body)
     (error (message "Error: %S" err)
	    (go-ext-kill-buffer ,buffer-name))))

(fext-defadvice go-import-add (around go-import-add (oldfun arg import))
  (interactive (cons current-prefix-arg (go-ext--package-complete)))
  (funcall oldfun arg import))

(defun go-ext--buffer-string-visible ()
  "Return the visible parts of the buffer."
  (cl-loop with temp = (generate-new-buffer " *temp*")
	   with beg = (point-min)
	   with end = (point-max)
	   while (/= beg end)
	   do
	   (when (invisible-p beg)	; better than (get-char-property beg 'invisible)?
	     (setq beg (next-single-char-property-change
			beg 'invisible nil end)))
	   (let* ((next (next-single-char-property-change
			 beg 'invisible nil end))
		  (substring (buffer-substring beg next)))
	     (with-current-buffer temp (insert substring))
	     (setq beg next))
	   finally return
	   (cl-ext-progn
	     (deactivate-mark)
	     (prog1 (with-current-buffer temp
		      (s-chop-suffix "\n" (buffer-string)))
	       (kill-buffer temp)))))

(defun go-ext-read-type (&optional prompt-prefix use-default)
  "Prompt the user for a type.
The prompt will start with PROMPT-PREFIX.  PROMPT-PREFIX
should not end with whitespace or a colon.  If PROMPT-PREFIX
is nil, it defaults to \"Type\".
Optional argument USE-DEFAULT a."
  (let ((types (append (go-ext-custom-types)
		       (go-ext-buffer-types)
		       (go-ext-builtin-types)))
	(prompt-prefix (or prompt-prefix "Type"))
	(prompt (if (and use-default
			 (car-safe user-ext-go-read-type-history))
		    (format "%s (%s): " prompt-prefix (car user-ext-go-read-type-history))
		  (format "%s: " prompt-prefix))))
    (completing-read prompt
		     types
		     nil
		     nil
		     nil
		     'user-ext-go-read-type-history
		     (and use-default (car-safe user-ext-go-read-type-history)))))


;; --- Godoc

(fext-defadvice godoc--read-query (override godoc--read-query)
  "Attempt to use LSP when possible."
  (let* ((default (car go-godoc-history)))
    (cl-symbol-macrolet
	((prompt (format "Godoc (default %s): " default))
	 (packages (go-packages)))
      (when-let ((_ (bound-and-true-p eglot--managed-mode))
		 (result (eglot-ext-symbol-at-point)))
	(setq default result))
      (if godoc-use-completing-read
	  (completing-read prompt packages nil nil nil
			   'go-godoc-history default)
	(read-from-minibuffer prompt nil nil nil
			      'go-godoc-history default)))))

(defun go-ext-godoc--markup-string (str)
  (with-temp-buffer
    (with-demoted-errors "Error during doc rendering: %S"
      (insert str)
      (delay-mode-hooks (go-mode))
      (ignore-errors (font-lock-ensure)))
    (go-ext--buffer-string-visible)))

(defun go-ext-godoc--parse-string (str)
  "Parse STR for a Godoc buffer.

This returns either a list of the form (GO-STRING POST-STRING)
or a string."
  (if (string-match
       (go-ext-rx string-start
		  (group "package"
			 (*? anything)
			 line-start
			 (or (seq "func" (+ nonl))
			     (seq "type"
				  (+? anything)
				  line-start
				  ?\}
				  line-end)))
		  (group (* anything)))
       str)
      (let ((gstr (match-string-no-properties 1 str))
	    (pstr (match-string-no-properties 2 str)))
	(go-ext-debug-body 0
	  (--print-expr var gstr)
	  (--print-expr var pstr))
	(list (go-ext-godoc--markup-string gstr) pstr))
    str))

(define-obsolete-function-alias 'go-ext-godoc-server--prompt-args
  'go-ext-godoc--prompt-args "2025-09-24")
(defun go-ext-godoc--read-args (prompt &optional arg)
  (thread-last
      (if arg
	  (read-string prompt
		       user-ext-go-godoc-default-args
		       'user-ext-go-godoc-args-history
		       user-ext-go-godoc-default-args)
	user-ext-go-godoc-default-args)
    (split-string-and-unquote)))

(defun go-ext-godoc (query &optional args)
  "Show Go documentation for QUERY."
  (interactive (list (godoc--read-query)
		     (go-ext-godoc--read-args "Godoc: " current-prefix-arg)))
  (cl-check-type query string)
  (let ((outbuf (get-buffer-create user-ext-go-godoc-output-buffer))
	(errbuf (get-buffer-create user-ext-go-godoc-error-buffer))
	(tmpfile (make-temp-file "godoc")))
    (go-ext-erase-buffers outbuf errbuf)
    (unwind-protect
	(when-let ((command (append (split-string-and-unquote godoc-command)
				    args
				    (list query)))
		   (insert-error
		    (lambda ()
		      (with-current-buffer errbuf
			(insert-file-contents tmpfile)
			(save-excursion
			  (goto-char (point-max))
			  (insert (format "\ncommand: %S [%S]"
					  (combine-and-quote-strings command)
					  command)))
			(activate-view-mode)))))
	  (setf (car command) (executable-find (car command)))
	  (or (car command)
	      (error "go binary not in PATH"))
	  (let ((default-directory (go-ext-project-root (go-ext-project-current))))
	    (pcase (call-process-shell-command
		    (combine-and-quote-strings command)
		    nil
		    (list outbuf tmpfile))
	      ((and (pred integerp)
		    code
		    (guard (/= code 0)))
	       (funcall insert-error)
	       (display-buffer errbuf t)
	       (error "Godoc exited with status %d" code))
	      ((and (pred stringp)
		    sigstr)
	       (error "Godoc received signal: %s" sigstr))))
	  (funcall insert-error)
	  (let (output)
	    (with-current-buffer outbuf
	      (let ((bstr (buffer-string)))
		(setq output
		      (pcase (go-ext-godoc--parse-string bstr)
			(`(,gstr ,pstr)
			 (concat gstr pstr))
			('nil (--print-expr sexp (go-ext-godoc--parse-string bstr))
			      (error "Failed to parse documentation string"))
			(str str))))
	      (erase-buffer)
	      (save-excursion (insert output))
	      (activate-view-mode))
	    (display-buffer outbuf t)))
      (with-demoted-errors "Error deleting temp file: %S"
	(go-ext-debug-body 1
	  (message "Deleting %s" tmpfile))
	(delete-file tmpfile)))))
(advice-add #'godoc :override #'go-ext-godoc)

(defun go-ext-godoc-server--running ()
  (when-let ((buffer (get-buffer user-ext-go-godoc-server-process-buffer))
	     (process (get-buffer-process buffer)))
    t))

(defun go-ext-godoc-server-stop ()
  "Stop a running Godoc server."
  (interactive)
  (when (go-ext-godoc-server--running)
    (go-ext-kill-buffer user-ext-go-godoc-server-process-buffer)))

(defun go-ext-godoc-server ()
  "Start Go documentation server.

The output is shown in a process buffer with the name taken
from `user-ext-go-godoc-server-process-buffer'."
  (interactive)
  (go-ext-kill-buffer-on-error user-ext-go-godoc-server-process-buffer
    (let* ((command
	    (append (split-string-and-unquote godoc-command)
		    (list "-http")))
	   (buffer (go-ext-go-process-run-command
		    command user-ext-go-godoc-server-process-buffer
		    :working-directory (go-ext-project-get-root))))
      (and (bufferp buffer)
	   (display-buffer buffer t)))))


;; --- Packages

(defun go-ext--package-complete ()
  (list
   (replace-regexp-in-string
    "^[\"']\\|[\"']$" ""
    (completing-read "Package: " (go-packages) nil nil nil 'user-ext-go-package-history))))

(defun go-ext-add-dependency (pkg)
  "Add PKG to the project using go get."
  (interactive (go-ext--package-complete))
  (go-ext-kill-buffer-on-error user-ext-go-process-buffer
    (let* ((command (append (split-string-and-unquote go-command)
			    (list "get" pkg)))
	   (buffer (go-ext-go-process-run-command
		    command user-ext-go-process-buffer
		    :working-directory (go-ext-project-get-root))))
      (and (bufferp buffer)
	   (display-buffer buffer t)))))


;; --- Docedit scratch buffer

(load-extension "go-subext_docedit")


;; --- Hs Mode Integration

(load-extension "go-subext_hs")


;; ### Project

(load-extension "go-subext_project")


;; ### Process buffer

(defconst go-ext-go-process-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map [remap self-insert-command] #'go-ext-process-no-edit)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "k") #'go-ext-process-kill-buffer)
    map))

(defun go-ext-process-no-edit ()
  "Prohibit the user from editing the buffer."
  (interactive)
  (user-error "Cannot to edit this buffer"))

(defun go-ext-process-kill-buffer ()
  "Kill the process associated with the current buffer.

Internally, this sends a SIGINT signal to the subprocess via
`comint-interrupt-subjob', then kills the buffer and quits.

This only works when the curent buffer's major mode is
`go-ext-go-process-mode'."
  (interactive)
  (let ((buffer (current-buffer)))
    (when (eq major-mode 'go-ext-go-process-mode)
      (with-demoted-errors "Kill buffer error: %S"
	(comint-interrupt-subjob))
      (while (comint-check-proc buffer)
	(sleep-for 0.1 100)))
    (kill-and-quit)))

(define-derived-mode go-ext-go-process-mode comint-mode
  "Go Process"
  "Major mode for Go processes.

\\{go-ext-go-process-mode-map}"
  (setq-local comint-process-echoes nil))

(cl-defun go-ext-go-process-run-command
    (command buffer-name &key working-directory query-on-exit)
  "Run COMMAND in a process buffer called BUFFER-NAME.
COMMAND is a list of the form (PROGRAM ARG...), where
PROGRAM is a string denoting an executable program, and each
ARG is an argument to PROGRAM.  If WORKING-DIRECTORY is
non-nil, set it as the working directory for the process,
otherwise use `default-directory'.

BUFFER-NAME and its process are killed intially before
creating a new one."
  (when-let ((buffer (get-buffer buffer-name))
	     (process (get-buffer-process buffer)))
    (and (buffer-live-p process)
	 (delete-process process)))
  (go-ext-kill-buffer buffer-name)
  (-let* ((buffer (get-buffer-create buffer-name))
	  ((program . args) command)
	  (program-name (f-filename program))
	  (wd (or working-directory default-directory)))
    (with-current-buffer buffer
      (go-ext-go-process-mode)
      (setq default-directory (file-name-as-directory wd)
	    header-line-format (format "Status: %%s | Working directory: %s"
				       default-directory)))
    (comint-exec buffer program-name program nil args)
    (run-with-idle-timer 0.1 nil
			 (lambda ()
			   (when-let ((process (get-buffer-process buffer)))
			     (cl-ext-progn
			       (set-process-query-on-exit-flag process query-on-exit)))))
    buffer))
(define-obsolete-function-alias 'go-ext-process-run-command
  'go-ext-go-process-run-command "2025-09-24")


;; ### Templates/Skeletons

(defun go-ext--before-ts-skeleton (&rest _r)
  "Assertions and error checks done before a skeleton."
  (go-ext-tree-sitter--assert-valid-state))

(defun go-ext-skeleton-no-newline (fun &rest r)
  "Set `skeleton-end-newline' to nil."
  (let (skeleton-end-newline)
    (apply fun r)))

(go-ext-define-auxillery-skeleton type-parameter-list
  nil nil
  ?\[ ("Type Parameter %s: "
       (unless (equal ?\[ (char-before)) ", ")
       str " " (skeleton-read "Constraint: "))
  resume:
  ?\])

(go-ext-define-skeleton function
  "Insert a function definition."
  "Name: "
  "func " str
  '(go-ext-skeleton--type-parameter-list)
  ?\( ("Parameter: "
       (unless (equal ?\( (char-before)) ", ")
       str ?\ (go-ext-read-type "Parameter Type" t))
  ?\) (progn (setq v1 (go-ext-read-type "Return Type"))
	     (unless (string-empty-p v1)
	       (concat " " v1)))
  " {" \n
  _ \n
  ?\} >)

(go-ext-define-skeleton test-function
  "Insert a test function."
  "Name: "
  "func Test" str "(t *testing.T) {" \n
  _ \n
  ?\} >)

(go-ext-define-skeleton lambda
  "Insert an anonymous function."
  str
  "func(" ("Parameter: "
	   (unless (equal ?\( (char-before)) ", ")
	   str ?\ (go-ext-read-type "Parameter Type" t))
  ?\) (progn (setq v1 (go-ext-read-type "Return Type"))
	     (and (not (string-empty-p v1))
		  (concat " " v1)))
  " {" \n
  _ \n
  ?\} >)
(advice-add 'go-ext-skeleton-lambda :around #'go-ext-skeleton-no-newline)

(go-ext-define-skeleton method
  "Insert a struct method."
  "Name: "
  "func (" (read-string "Receiver: " nil t)
  ?\  (go-ext-read-type "Receiver Type" t)
  ") " str ?\( ("Parameter: "
		(unless (equal ?\( (char-before)) ", ")
		str ?\ (go-ext-read-type "Parameter Type" t))
  ?\) (progn (setq v1 (go-ext-read-type "Return Type"))
	     (and (not (string-empty-p v1))
		  (concat " " v1)))
  " {" \n _ \n ?\} >)
(advice-add 'go-ext-skeleton-method :before #'go-ext--before-ts-skeleton)

(go-ext-define-skeleton package-main
  "Insert a package main."
  nil
  "package main" \n \n
  "func main() {" \n
  _ \n
  ?\} >)

(go-ext-define-skeleton struct
  "Insert a struct definition."
  "Name: "
  "type " str " struct {" \n
  ("Field Name: " str ?\  (go-ext-read-type "Field Type" t) \n)
  ?\} >)

(go-ext-define-skeleton anonymous-struct
  "Insert an anonymous struct."
  nil
  "struct {" \n
  ("Field Name: " str ?\  (read-string "Type: " nil t) \n)
  ?\} >)

(go-ext-define-skeleton interface
  "Insert an integerface."
  "Name: "
  "type " str " interface {" \n
  ("Method Name: " str ?\( ("(Optional Parameter) & Type: "
			    (unless (equal ?\( (char-before)) ", ")
			    str)
   ?\) \n)
  resume:
  ?\} >)

(go-ext-tempo-define-template "go-file"
  "Insert a Go file for the current package."
  ("package " (go-ext-project-package) \n
   \n r \n))

(go-ext-tempo-define-template "test-file"
  "Insert a test file for the current package."
  ("package " (go-ext-project-package) \n
   \n "import \"testing\"" \n))


;; ### Keymaps

(eval-when-compile
  (defvar user-ext-go-skeleton-map)
  (defvar user-ext-go-hide-map)
  (declare-function elisp-ext-yank-and-indent "elisp-ext"))

(define-prefix-command 'user-ext-go-skeleton-map)
(keymaps-ext-set-keymap go-mode-map "C-c C-s" #'user-ext-go-skeleton-map)
(keymaps-ext-set-keymap user-ext-go-skeleton-map "f" #'go-ext-skeleton-function)
(keymaps-ext-set-keymap user-ext-go-skeleton-map "m" #'go-ext-skeleton-method)
(keymaps-ext-set-keymap user-ext-go-skeleton-map "s" #'go-ext-skeleton-struct)
(keymaps-ext-set-keymap user-ext-go-skeleton-map "M-s" #'go-ext-skeleton-anonymous-struct)
(keymaps-ext-set-keymap user-ext-go-skeleton-map "i" #'go-ext-skeleton-interface)
(keymaps-ext-set-keymap user-ext-go-skeleton-map "l" #'go-ext-skeleton-lambda)

(define-prefix-command 'user-ext-go-hide-map)
(keymaps-ext-set-keymap go-mode-map "C-c f" #'user-ext-go-hide-map)
(keymaps-ext-set-keymap user-ext-go-hide-map "b" #'go-ext-hide-block)
(keymaps-ext-set-keymap user-ext-go-hide-map "s" #'go-ext-hide-struct)
(keymaps-ext-set-keymap user-ext-go-hide-map "C-s" #'go-ext-hide-all-structs)
(keymaps-ext-set-keymap user-ext-go-hide-map "C-i" #'go-ext-hide-all-interfaces)
(keymaps-ext-set-keymap user-ext-go-hide-map "f" #'go-ext-hide-function)
(keymaps-ext-set-keymap user-ext-go-hide-map "C-f" #'go-ext-hide-all-functions)
(keymaps-ext-set-keymap user-ext-go-hide-map "m" #'go-ext-hide-method)
(keymaps-ext-set-keymap user-ext-go-hide-map "@" #'hs-show-block)

(keymaps-ext-set-keymap go-mode-map "C-c C-f s" #'go-ext-goto-struct)
(keymaps-ext-set-keymap go-mode-map [remap go-goto-function] #'go-ext-goto-function-or-method)
(keymaps-ext-set-keymap go-mode-map [remap go-goto-function-name] #'go-ext-goto-function-or-method)
(keymaps-ext-set-keymap go-mode-map "C-c C-f p" #'go-ext-goto-package)
(keymaps-ext-set-keymap go-mode-map "C-c C-u" #'go-ext-goto-block)

(keymaps-ext-set-keymap go-mode-map "M-SPC" #'company-capf)

(keymaps-ext-set-keymap go-mode-map "C-c <delete>" #'go-ext-delete-dwim)


;; --- Menu

(easy-menu-define user-ext-go-menu-map
  go-mode-map
  "Go Extension"
  '("Go Extension"
    ["Go Tree Sitter" go-ext-tree-sitter-mode
     :style toggle :selected go-ext-tree-sitter-mode]
    "---"
    ("Hide/Show"
     :active go-ext-tree-sitter-mode
     ["Hide Function" go-ext-hide-function]
     ["Hide All Functions" go-ext-hide-all-functions]
     ["Hide Struct" go-ext-hide-struct]
     ["Hide All Structs" go-ext-hide-all-structs])
    ("Up Movement"
     :active go-ext-tree-sitter-mode
     ["Struct" go-ext-goto-struct]
     ["Function " go-ext-menu--goto-function
      :keys "\\[go-goto-function]"]
     ["Function Name" go-ext-menu--goto-function-name
      :keys "C-c C-f n"]
     ["Package Declaration" go-ext-goto-package]
     ["Block" go-ext-goto-block])
    ("Project"
     ["Find File" project-find-file]
     ["Rename Buffer" go-ext-project-rename-buffer])
    ("Skeletons"
     ["Struct" go-ext-skeleton-struct]
     ["Anonymouse Struct" go-ext-skeleton-anonymous-struct]
     ["Interface" go-ext-skeleton-interface]
     ["Function" go-ext-skeleton-function]
     ["Test Function" go-ext-skeleton-test-function]
     ["Anonymous Function" go-ext-skeleton-lambda]
     ["Method" go-ext-skeleton-method
      :active go-ext-tree-sitter-mode])
    "---"
    ["Edit Doc Comment" go-ext-docedit]
    "---"
    ["Godoc" go-ext-godoc]
    ("Godoc Server"
     ["Start Godoc Server" go-ext-godoc-server
      :active (not (go-ext-godoc-server--running))]
     ["Stop Active Godoc Server" go-ext-godoc-server-stop
      :active (go-ext-godoc-server--running)])))

(defun go-ext-menu--goto-function ()
  (interactive)
  (let ((last-command-event ?f))
    (go-ext-goto-function-or-method)))

(defun go-ext-menu--goto-function-name ()
  (interactive)
  (let ((last-command-event ?n))
    (go-ext-goto-function-or-method)))


;; ### Mode Hook

;;;###autoload
(defun go--extra-hook ()
  (setq-local indent-tabs-mode user-ext-go-indent-tabs
	      tab-width user-ext-go-tab-width
	      tempo-interactive t
	      tempo-user-elements (cons #'tempo-ext-tempo-handler
					tempo-user-elements))
  (add-hook 'before-save-hook #'gofmt-before-save nil t)
  (go-ext-tree-sitter-mode 1)
  (tree-sitter-hl-mode 1))

;;;###autoload
(add-hook 'go-mode-hook #'go--extra-hook)

(extension-provide 'go-ext user-ext-go-subextensions)
;;; go-ext.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-go")
;; eval: (abbrev-ext-define-local-abbrev "uxg" "user-ext-go-godoc")
;; eval: (abbrev-ext-define-local-abbrev "gx" "go-ext")
;; eval: (abbrev-ext-define-local-abbrev "gxp" "go-ext-project")
;; eval: (abbrev-ext-define-local-abbrev "gxg" "go-ext-godoc")
;; eval: (abbrev-ext-define-local-abbrev "gxgp" "go-ext-go-process")
;; eval: (abbrev-ext-define-local-abbrev "gts" "go-ext-tree-sitter")
;; eval: (abbrev-ext-define-local-abbrev "tse" "tree-sitter-ext")
;; eval: (local-lambda-define-local-defun eval-test-hide-all-x nil "Test: eval `go-ext-define-hide-all-x-function'. In order to work, the following setup is required: - At least two tabs: left one in go-ext.el, split in two windows, one with the point to where it can evaluate a defining form, the other with the point where it can evaluate my \"prettyprint function def in other window\" form." (interactive) (let ((tl (syntax-ppss-toplevel-pos (make-ppss-easy (syntax-ppss)))) ow) (when tl (save-excursion (goto-char tl) (elisp-ext-forward-or-backward-sexp) (call-interactively #'eval-last-sexp)) (tab-previous) (and (y-or-n-p "Other window? ") (setq ow t) (call-interactively #'other-window)) (call-interactively #'eval-last-sexp) (call-interactively #'other-window) (call-interactively #'eval-last-sexp))))
;; End:
