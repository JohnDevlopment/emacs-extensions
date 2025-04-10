;; -*- lexical-binding: t; -*-

(defconst user-ext-extension-directory "~/.emacs.d/extensions")

(add-to-list 'load-path "~/.emacs.d/extensions")
(add-to-list 'load-path "~/.emacs.d/extensions/packages")

(require 'cl-lib)

(eval-when-compile
  (require 'alist-ext)
  (require 'bind-key)
  (require 'cl-ext))

;; ---External packages via `use-package'

(use-package f
  :functions
  f-join
  f-exists-p
  f-glob
  f-newer-p)

(use-package embed-doc
  :autoload
  embed-doc-document-symbol
  embed-doc-get-documentation)

;; ---

(setq read-process-output-max 10485760
      frame-title-format (concat (and multiple-frames ()) " %b " invocation-name "@" (system-name)))

(put 'narrow-to-region 'disabled nil)

;; Functions

;; ---Loading/finding extensions

(defun --extension-completion (prompt &optional initial-input)
  (let* ((path "~/.emacs.d/extensions")
	 (args (list (completing-read
		      prompt
		      (apply-partially #'locate-file-completion-table
				       (list path)
				       (get-load-suffixes))
		      nil nil initial-input)))
	 completion-ignored-extensions)
    (when current-prefix-arg
      (push (y-or-n-p "Load safely? ") args)
      (push (read-number "Defer seconds: " 0) args))
    (nreverse args)))

(defun load-extension (extension &optional safe defer)
  "Load EXTENSION.
EXTENSION is a Lisp file under '~/.emacs.d/extensions' without its
file extension.  For example, with an extension named 'general',
the file '~/.emacs.d/extensions/general.el' will be loaded.

If SAFE is non-nil, demote errors to simple messages.  If DEFER is
non-nil, it is an integer specifying how many seconds of idle time
to wait before loading EXTENSION.

Interactively, prompt the user for EXTENSION with completion.  With
the prefix argument, also prompt the user for SAFE and DEFER."
  (interactive (--extension-completion "Load Extenion: "))
  (cl-check-type extension string)
  (cl-check-type defer integer-or-null)
  (let ((file (f-join "~/.emacs.d/extensions/" extension))
	(msg (concat "Error loading " extension ": %S"))
	(dmsg (cl-ext-when (and defer (> defer 0))
		(format "Loading %s in %d seconds..." extension defer)))
	(safe-load (lambda (f msg)
		     (with-demoted-errors msg
		       (load f)))))
    (cond ((and safe defer (> defer 0))
	   (message dmsg)
	   (run-with-idle-timer defer nil safe-load file msg))
	  (safe
	   (funcall safe-load file msg))
	  (defer
	    (message dmsg)
	    (run-with-timer defer nil #'load file))
	  (t (load file))))
  t)

(defmacro load-extension-safe (extension &optional defer)
  "Load EXTENSION, capturing any error and displaying it as a message."
  (cl-check-type extension string)
  (cl-check-type defer integer-or-null)
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
    (cl-ext-when (or (f-exists-p file)
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
			     (cl-ext-when (stringp thing) thing))))
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

;; ---

(defmacro eval-after-require (feature &rest body)
  "Attempt to load FEATURE and eval BODY if it succeeds.
If the file provuding FEATURE cannot be found, an error
message is provided.  On success, the BODY forms are
evaluated."
  (declare (indent 1) (debug (sexp body)))
  `(progn
     (condition-case err
	 (prog1 (require (quote ,feature))
	   ,@body)
       (file-missing (message "Failed to load %S: %S" (quote ,feature) err)))))

(eval-after-require embed-doc
  (embed-doc-document-symbol
   extensions
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

;; Autoloads
(load-extension-safe "loaddefs-ext")

(load-extension "types-ext")
(load-extension "syntax-ext")
(load-extension "errors")
(load-extension "general")
(load-extension "macro-ext")
(load-extension "buffers-ext")

;; Bootstraps for external packages
(load-extension-safe "code-outline-bootstrap")
(load-extension-safe "jdesktop-bootstrap")
(load-extension-safe "liquidsoap-bootstrap")

(load-extension-safe "abbrev-ext")
(load-extension-safe "codeium-ext")
(load-extension-safe "desktop-ext")
(load-extension "custom-ext")
(load-extension "help-ext")

(load-extension-safe "lsp-ext" 2)
(load-extension-safe "desktop-ext" 2)
(load-extension "dired-ext")
(load-extension-safe "imenu-ext" 2)

(load-extension "keymaps-ext")
(load-extension-safe "yasnippets-ext")

(provide 'extensions)
;;; extensions.el ends here
