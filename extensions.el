;; -*- lexical-binding: t; -*-

(defconst user-ext-extension-directory "~/.emacs.d/extensions")

(defconst user-ext-dependencies
  '(use-package yasnippet))

(add-to-list 'load-path "~/.emacs.d/extensions")
(add-to-list 'load-path "~/.emacs.d/extensions/packages")

(require 'cl-lib)

(eval-when-compile
  (require 'debug-ext)
  (require 'alist-ext)
  (require 'bind-key)
  (require 'cl-ext)
  (require 'subr-x))


;; ### Initialization

(eval-and-compile
  (require 'embed-doc)
  (embed-doc-document-symbol extensions
    "Main loader for extensions."
    :functions
    eval-after-require
    load-extension-safe
    :variables
    user-ext-extension-directory
    :commands
    find-extension
    find-extension-at-point
    get-extension-documentation
    load-extension))


;; --- `use-package' for autoloads

(use-package f
  :defer t
  :autoload
  f-join
  f-exists-p
  f-glob
  f-newer-p)

(use-package s
  :defer t
  :autoload
  s-lex-format
  s-lex-fmt|expand
  s-lex-value-as-lisp)

(use-package embed-doc
  :defer t
  :autoload
  embed-doc-document-symbol
  embed-doc-get-documentation)

(use-package mode-local
  :autoload
  defvar-mode-local)


;; --- Configurations (i.e., variables, function properties, etc.)

(setq read-process-output-max 10485760
      frame-title-format (concat (and multiple-frames ()) " %b "
				 invocation-name "@"
				 (system-name))
      Info-directory-list (cons (expand-file-name "~/.local/share/info")
				Info-directory-list))

(function-put #'narrow-to-region 'disabled nil)


;; ### Variables

(defvar extension-history nil "HIstory variable for extension functions.")

(defconst extension-warnings-buffer "*Extension Warnings*"
  "Name of the warnings buffer for `extensions'.")

(defconst extension-features nil
  "A list of symbols which are the loaded extensions.
Used by `extensionp' and modified by `extension-provide'.")


;; ### Functions


;; --- Helpers

(define-error 'extension-error "Extension error")
(define-error 'extension-disabled "Disabled extension" 'extension-error)
(define-error 'extension-emacs-version-error "Wrong Emacs version" 'extension-error)

(defsubst signal-extension-error (msg)
  "Signal an `extension-error' with MSG (a string) as the message."
  (signal 'extension-error msg))

(defun signal-emacs-version-error (version cmp)
  "Signal `extension-emacs-version-error' with VERSION and CMP.
VERSION (a string) is the target Emacs version.
CMP, a symbol, indicates the comparison done between the
Emacs version and VERSION.

CMP can be one of the following:
- `<':  Emacs is older than VERSION
- `<=': Emacs is older than or equal to VERSION
- `>':  Emacs is newer than VERSION
- `>=': Emacs is newer than or equal to VERSION
- `=':  Emacs is not exactly at version VERSION"
  (cl-check-type version string)
  (cl-check-type cmp symbol)
  (cl-assert (memq cmp '(= < <= > >=)) t)
  (signal 'extension-emacs-version-error (list emacs-version cmp version)))

(defun --emacs-version-cmp (version cmp)
  (cl-case cmp
    (= `(version= emacs-version ,version))
    (< `(version<= emacs-version ,version))
    (<= `(version< emacs-version ,version))
    (> `(version< ,version emacs-version))
    (>= `(version<= ,version emacs-version))
    (t (error "Invalid CMP %S" cmp))))

(defmacro check-emacs-minimum-version (version)
  "Declare that this file's minimum required Emacs version is VERSION.
VERSION, a string, denotes the minimum required version for
the containing file to work.
If the Emacs version is older than VERSION, then signal
`extension-emacs-version-error'."
  (cl-check-type version string)
  `(unless (version<= ,version emacs-version)
     (signal-emacs-version-error ,version '>=)))

(defmacro with-emacs-version (cmp version &rest body)
  "Do BODY if the Emacs version matches VERSION with comparison CMP.
VERSION, a string, denotes the target version (e.g., \"27.1\").
CMP, a symbol, indicates the relationship between the Emacs
version EVER and VERSION.
In any case, if EVER, when compared with VERSION using CMP,
matches, then do BODY.

CMP can be one of:
- `=':  EVER must be equal to VERSION
- `<':  EVER must be older than VERSION
- `<=': EVER must be older than or the same as VERSION
- `>':  EVER must be newer than VERSION
- `>=': EVER must be newer than or the same as VERSION"
  (declare (indent 2) (debug ([&or "=" "<=" ">="]
			      stringp
			      &rest form)))
  `(when ,(--emacs-version-cmp version cmp)
     ,@body))

(defmacro emacs-version-cond (&rest conditions)
  "Try each clause, matching the Emacs version, until one succeeds.
Each clause has the form ((CMP VERSION) BODY...).  The Emacs
version is compared against VERSION (a string) and, if the
comparison evaluates to non-nil, this clause succeeds, then
the expressions in BODY are evaluated and the last one's
value is the value of this form.
If no clause succeeds, this form returns nil.

The comparison operator CMP indicates what type of
comparison is done.  See `with-emacs-version' for the
description of CMP.

The entire block is wrapped in an implicit nil block, so
`cl-return' can be used in any of the clauses.

\(fn CLAUSES...)"
  (declare (indent defun))
  (let (new-conditions)
    (setq new-conditions
	  (cl-loop
	   for condition in conditions
	   collect
	   (progn
	     (pcase condition
	       (`((,cmp ,version) . ,body)
		(cons (--emacs-version-cmp version cmp) body))
	       (`(t . ,body)
		(cons t body))
	       (_ (error "Invalid form: %S" condition))))))
    `(cl-block nil
       (cond ,@new-conditions))))

(defmacro emacs-version-cond-when-compile (&rest conditions)
  "The compile-time counterpart to `emacs-version-cond'.
The main difference is that this macro tries each clause at
compile time and expands the body for the succeeding clause.

Consider an example.  If the current Emacs version is older
than or equal to 27.4, then the following will expand to 1.

   (emacs-version-cond-when-compile
     ((<= \"27.4\") 1)
     (> \"27.4\" 2)
     (t 3))

On versions newer than 27.4, the snippet will expand to 2,
and on any other version will expand to 3.

\(fn CLAUSES...)"
  (declare (indent defun))
  (let ((code
	 (cl-loop
	  for condition in conditions
	  do
	  (pcase condition
	    (`((,cmp ,version) . ,body)
	     (when (eval (--emacs-version-cmp version cmp))
	       (cl-return body)))
	    (`(t . ,body)
	     (cl-return body))
	    (_ (error "Invalid form: %S" condition))))))
    `(progn ,@code)))

(defsubst signal-disabled (extension)
  "Signal that EXTENSION is disabled."
  (signal 'extension-disabled (list extension)))

(defun extension-provide (extension &optional subextensions)
  "Announce that EXTENSION is loaded.
The optional argument SUBEXTENSIONS should be a list of
symbols listing subextensions which are also loaded."
  (unless (memq extension extension-features)
    (prog1 extension
      (setf (alist-get extension extension-features) subextensions))))

(defun extensionp (extension &optional subextension)
  "Return non-nil if EXTENSION is loaded.

Use this to conditionalize execution of Lisp code based on
whether EXTENSION is loaded or not.
Use `extension-provide' to declare that an extension is loaded.
This function looks at the value of `extension-features'.
The optional argument SUBEXTENSION can be used to check for
a specific subextension."
  (declare (side-effect-free t))
  (when-let ((ext (assq extension extension-features)))
    (if subextension
	(cl-ext-progn
	  (and subextension
	       (and (-elem-index subextension ext) t)))
      (and (car ext) t))))

(defmacro extension-check-requires (&rest requirements)
  "Declare that EXTENSION depends on REQUIREMENTS.
If any of the packages in REQUIREMENTS is neither loaded nor
available, signal an error and list the missing packages in
the buffer `extension-warnings-buffer'.
This is meant to be used inside the extension in question,
to tell `load-extension' that it depends on one or more
packages.  If even one of the packages is not available,
then the extension cannot load."
  (declare (debug (&rest symbol)))
  (let ((curext
	 (and-let* ((_ (f-same-p default-directory user-ext-extension-directory))
		    (fn (-some--> (buffer-file-name)
			  (f-filename it)
			  (f-base it))))
	   (intern-soft fn))))
    `(extension-check-requires-1 ',curext ',requirements)))

(defun extension-check-requires-1 (extension packages)
  "Internal function."
  (cl-check-type extension symbol)
  (cl-check-type packages list)
  (cl-loop with warnings-displayed = 0
	   for package in packages
	   do
	   (or (featurep package)
	       (require package nil t)
	       (and (cl-incf warnings-displayed)
		    (display-warning 'user-extensions
				     (format "Failed to load `%S'" package)
				     :error
				     extension-warnings-buffer)))
	   finally do
	   (when (> warnings-displayed 0)
	     (signal-extension-error
	      (format "Failed to load `%S'" extension)))))

(defun --list-extensions (&optional suffix completion)
  (let* ((regex (rx string-start
		    (any (?a . ?z))
		    (+ (any "a-z0-9-"))
		    ".el" (opt ".gz")
		    string-end))
	 (regex2 (rx ".el" (opt ".gz") string-end))
	 (files (thread-last
		    (directory-files "~/.emacs.d/extensions")
		  (cl-remove-if-not (lambda (file)
				      (string-match-p regex file)))))
	 (cf (lambda (file) (replace-regexp-in-string regex2 "" file))))
    (if completion
	(cl-ext-progn
	  (completion-table-dynamic
	   (lambda (string) (all-completions string (mapcar cf files)))))
      (if suffix
	  (mapcar cf files)
	files))))

(defun --extension-completion (prompt &optional initial-input)
  (let* ((args (list (completing-read
		      prompt
		      (--list-extensions t t)
		      nil nil initial-input
		      'extension-history)))
	 completion-ignored-extensions)
    (when current-prefix-arg
      (push (y-or-n-p "Load safely? ") args)
      (push (read-number "Defer seconds: " 0) args))
    (nreverse args)))

(defun load-extension (extension &optional safe defer)
  "Load EXTENSION.
EXTENSION is a Lisp file under \\=`~/.emacs.d/extensions' without its
file extension.  For example, with an extension named \\=`general',
the file \\=`~/.emacs.d/extensions/general.el' will be loaded.

If SAFE is non-nil, demote errors to simple messages.  If DEFER is
non-nil, it is an integer specifying how many seconds of idle time
to wait before loading EXTENSION.

Interactively, prompt the user for EXTENSION with completion.  With
the prefix argument, also prompt the user for SAFE and DEFER."
  ;; TODO: update documentation string
  (interactive (--extension-completion "Load Extenion: "))
  (cl-check-type extension string)
  (cl-check-type defer (or (integer 1 *) (float 0.01 *) null))
  (let* ((file (f-join "~/.emacs.d/extensions/" extension))
	 (dmsg (and defer
		    (> defer 0)
		    (format "Loading %s in %d seconds..." extension defer)))
	 (safe-load (lambda (f) (condition-case err
				    (load f)
				  (extension-disabled
				   (message "%S is disabled" (cdr-safe err)))
				  (error
				   (message "Error loading %s: %S" f err)))))
	 (warning-suppress-types (cons warning-suppress-types
				       '(comp))))
    (prog1 t
      (cond ((and safe defer (> defer 0))
	     (message dmsg)
	     (run-with-idle-timer defer nil safe-load file))
	    (safe
	     (funcall safe-load file))
	    (defer
	      (message dmsg)
	      (run-with-timer defer nil #'load file))
	    (t (condition-case-unless-debug err
		   (load file)
		 (extension-disabled
		  (display-warning 'extensions
				   "%S is disabled"
				   (cdr-safe err)))))))))

(defmacro load-extension-safe (extension &optional defer requires)
  "Load EXTENSION, capturing any error and displaying it as a message."
  (declare (advertised-calling-convention (extension &optional defer)
					  "2026-01-09"))
  (cl-check-type extension string)
  (cl-check-type defer (or (integer 1 *) (float 0.01 *) null))
  (cl-check-type requires (or list null))
  `(load-extension ,extension t ,defer))

(defun --extension-choose-file (files)
  (if (> (length files) 0)
      (let* ((files (cl-remove-if
		     (lambda (x) (string-match-p "\\.elc$" x)) files)))
	(car (if (= (length files) 1)
		 files
	       (cl-sort files #'f-newer-p))))
    nil))

(defun find-extension (extension)
  "Find the Emacs Lisp source of EXTENSION.

Interactively, prompt for EXTENSION."
  (interactive (--extension-completion "Find Extension: "))
  (require 's)
  (let* ((dir "~/.emacs.d/extensions/")
	 (files (f-glob (concat (f-join dir extension) "*")))
	 (file (or (--extension-choose-file files)
		   (concat (f-join dir extension) ".el"))))
    (cl-assert file)
    (when (or (f-exists-p file)
	      (yes-or-no-p
	       (s-lex-format "${file} does not yet exist. Create it? ")))
      (switch-to-buffer (find-file-noselect file)))))

(defun find-extension-at-point (extension)
  "Find the Emacs Lisp source of EXTENSION at point.

Interactively, prompt for EXTENSION using the one at or near
point."
  (interactive
   (let* ((thing (thing-at-point 'sexp))
	  (end (if (stringp thing) (length thing))))
     (set-text-properties 0 end nil thing)
     (--extension-completion "Find Extension: "
			     (and (stringp thing) thing))))
  (prog1
      (switch-to-buffer (find-file-noselect
			 (concat "~/.emacs.d/extensions/" extension ".el")))))

(defun get-extension-documentation (extension)
  "Display the full documentation EXTENSION."
  (interactive (--extension-completion "Get Help For Extension: "))
  (let* ((extension-symbol (intern-soft extension))
	 (doc (embed-doc-get-documentation extension-symbol)))
    (cond ((not extension-symbol)
	   (user-error "Failed to intern symbol for %S" extension))
	  ((not doc)
	   (user-error "No documentation exists for %s" extension))
	  (t
	   (with-help-window (help-buffer)
	     (princ doc)
	     (help-setup-xref (list 'get-extension-documentation
				    extension)
			      (called-interactively-p 'interactive)))))))

(defmacro eval-after-require (feature &optional first-form &rest body)
  "Attempt to load FEATURE and eval BODY if it succeeds.
If the file provuding FEATURE cannot be found, an error
message is provided.  On success, the BODY forms are
evaluated.

\(fn FEATURE [IGNORE-ERRORS] BODY...)"
  (declare (indent defun) (debug (symbolp [&optional booleanp] body)))
  `(progn
     ,@(if (and (booleanp first-form) first-form)
	   `((ignore-errors
	       (prog1 (require (quote ,feature))
		 ,@body)))
	 `((condition-case err
	       (prog1 (require (quote ,feature))
		 ,first-form
		 ,@body)
	     (file-missing (message "Failed to load %S: %S" (quote ,feature) err)))))))

(defun --make-sure-user-actually-wants-to-quit ()
  (y-or-n-p-with-timeout
   (substitute-command-keys
    "You hit \\[save-buffers-kill-terminal]. Did you mean to quit Emacs (default n in 3 seconds)? ")
   3 nil))

;; shortdoc
(with-emacs-version >= "28.1"
  (define-short-documentation-group user-extensions/main
    "Finding & Loading"
    (load-extension
     :no-value (load-extension "types")
     :no-manual t)
    (find-extension
     :no-value (find-extension "types")
     :no-manual t)
    (find-extension-at-point
     :no-value (find-extension-at-point "types")
     :no-manual t)
    (get-extension-documentation
     :no-value (get-extension-documentation "types")
     :no-manual t)
    "Errors"
    (signal-disabled
     :no-value (signal-disabled 'types)
     :no-manual t)
    (signal-emacs-version-error
     :no-value (signal-emacs-version-error "30" '<=)
     :no-manual t)
    (signal-extension-error
     :no-value (signal-extension-error "Some error")
     :no-manual t)
    (extension-check-requires
     :no-value (extension-check-requires foo)
     :no-manual t)
    (check-emacs-minimum-version
     :no-value (check-emacs-minimum-version "30.4")
     :no-manual t)
    "Helpers"
    (extension-provide
     :no-eval (extension-provide 'types)
     :result types
     :no-manual t)
    (extensionp
     :no-eval* (extensionp 'types)
     :no-manual t)
    (emacs-version-cond
      :no-value (emacs-version-cond
		  ((>= "29.4")
		   (message "Version 29.4 or newer"))
		  (t (message "Older than version 29.4")))
      :no-manual t)
    (emacs-version-cond-when-compile
      :no-eval* (emacs-version-cond-when-compile
		  ((>= "29.4")
		   (message "Version 29.4 or newer"))
		  (t (message "Older than version 29.4")))
      :no-manual t)
    (with-emacs-version
	:no-eval* (with-emacs-version >= "29.4"
		    (message "Using Emacs version 29.4 or newer"))
      :no-manual t)
    (eval-after-require
      :eval (eval-after-require f
	      (message "File: %s" (f-join "/tmp" "foo")))
      :no-manual t)))


;; --- Autoloads

(load-extension-safe "loaddefs-ext")


;; ### Loading extensions

;; --- Base Extensions

(load-extension "types")
(load-extension "errors")
(load-extension "general")
(emacs-version-cond
  ((< "29.1")
   (unless (featurep 'use-package)
     (display-warning
      'user-extensions
      "`use-package' is required")
     (cl-return))
   (load-extension-safe "keymaps-ext"))
  ((>= "29.1")
   (load-extension-safe "keymaps-ext")))


;; --- Bootstraps for external packages


;; --- Extensions

(load-extension "compat-28-ext")
(load-extension "compat-29-ext")
(load-extension "abbrev-ext")
(load-extension "buffers-ext")
(load-extension "syntax-ext")
(load-extension "hs-ext")
(load-extension "subr-ext")
(load-extension "thingatpt-ext" nil 1)
(load-extension "ibuffer-ext" nil 1)
(load-extension "tempo-ext" nil 1)
(load-extension-safe "yasnippets-ext" 1)
(load-extension-safe "macro-ext" 2)

(extension-provide 'extensions)
;;; extensions.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "knm" ":no-manual")
;; eval: (abbrev-ext-define-local-abbrev "knv" ":no-value")
;; eval: (abbrev-ext-define-local-abbrev "ke" ":eval")
;; eval: (abbrev-ext-define-local-abbrev "kne" ":no-eval")
;; eval: (abbrev-ext-define-local-abbrev "kr" ":result")
;; eval: (abbrev-ext-define-local-abbrev "ker" ":eg-result")
;; eval: (abbrev-ext-define-local-abbrev "kr" ":result")
;; End:
