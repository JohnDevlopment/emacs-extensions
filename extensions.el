;; -*- lexical-binding: t; -*-

(defconst user-ext-extension-directory "~/.emacs.d/extensions")

(add-to-list 'load-path "~/.emacs.d/extensions")
(add-to-list 'load-path "~/.emacs.d/extensions/packages")

(eval-when-compile
  (require 'alist-ext)
  (require 'bind-key)
  (require 'cl-ext))

(require 'cl-lib)
(require 'f)

(setq read-process-output-max 10485760
      frame-title-format (concat (and multiple-frames ()) " %b " invocation-name "@" (system-name))
      source-directory "~/github/emacs/src/")


;; Types

(cl-deftype list-or-null () '(or list null))
(cl-deftype marker-or-null () '(or marker null))
(cl-deftype integer-or-null () '(or integer null))
(cl-deftype string-or-null () '(or string null))

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
	   (run-with-timer defer nil safe-load file msg))
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

(defun find-extension (extension)
  "Find the Emacs Lisp source of EXTENSION.

Interactively, prompt for EXTENSION."
  (interactive (--extension-completion "Find Extension: "))
  (prog1
      (switch-to-buffer (find-file-noselect
			 (concat "~/.emacs.d/extensions/" extension ".el")))))

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
  (let ((var (intern-soft (format "user-ext-%s-documentation" extension))))
    (cl-ext-unless var
      (user-error "No documentation exists for %s" extension))
    (describe-variable var)))

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

;; Autoloads
(load-extension "loaddefs-ext" t)

(load-extension-safe "abbrev-ext")
(load-extension-safe "codeium-ext")
(load-extension-safe "desktop-ext")
(load-extension "custom-ext")

(load-extension "general")
(load-extension "macro-ext")

(load-extension-safe "lsp-ext" 2)
(load-extension-safe "desktop" 2)
(load-extension "dired-ext")
(load-extension "buffers-ext")
(load-extension "imenu-ext" nil 2)
(load-extension "syntax-ext")

(load-extension "keymaps-ext")
(load-extension-safe "yasnippets-ext")

;; Bootstraps for external packages
(load-extension "code-outline-bootstrap" nil 2)
(load-extension "liquidsoap-bootstrap" nil 2)

(provide 'extensions)
;;; extensions.el ends here
