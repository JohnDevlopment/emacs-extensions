;; -*- lexical-binding: t; -*-

(defconst user-ext-extension-directory "~/.emacs.d/extensions")

(add-to-list 'load-path "~/.emacs.d/extensions")
(add-to-list 'load-path "~/.emacs.d/extensions/packages")

(require 'cl-lib)
(require 'cl-ext)
(require 'alist-ext)
(require 'debug-ext)
(require 'documentation-ext)

(setq read-process-output-max 10485760)

(setq frame-title-format
      (concat multiple-frames " %b " invocation-name "@" (system-name)))

(document-extension "extensions"
  "The main loader for custom extensions."
  :functions
  (document-extension eval-after-require
    find-extension
    find-extension-at-point
    get-extension-documentation
    load-extension
    load-extension-safe)
  :variables
  ((user-ext-extension-directory constant)))

;; Enable `narrow-to-region'
(put 'narrow-to-region 'disabled nil)

;; ---Loading/finding extensions

(defun --extension-completion (&optional initial-input)
  (let* (completion-ignored-extensions
	 (path "~/.emacs.d/extensions"))
    (list (completing-read "Load extension: "
			   (apply-partially 'locate-file-completion-table
					    (list path)
					    (get-load-suffixes))
			   nil nil initial-input))))

(defun load-extension (extension &rest _args)
  "Load an extension.

EXTENSION is a file '~/.emacs.d/extensions' without its file
extension.  For example, with an extension named 'general',
the file '~/.emacs.d/extensions/general.el' is loaded."
  (interactive (--extension-completion))
  (load (concat "~/.emacs.d/extensions/" extension)))
(set-advertised-calling-convention 'load-extension '(extension) "2025.02.02")

(defmacro load-extension-safe (extension)
  "Load EXTENSION.
This calls `load-extension' but captures the error condition
`file-missing'.  If such an error occurs, the resulting
error is demoted to a simple message."
  (cl-check-type extension string)
  `(condition-case err (load-extension ,extension)
     (file-missing
      (message "%s" (error-message-string err)))))

(defun find-extension (extension)
  "Find the Emacs Lisp source of EXTENSION."
  (interactive (--extension-completion))
  (prog1
      (switch-to-buffer (find-file-noselect
			 (concat "~/.emacs.d/extensions/" extension ".el")))))

(defun find-extension-at-point (extension)
  (interactive
   (let* ((thing (thing-at-point 'sexp))
	  (end (if (stringp thing) (length thing))))
     (set-text-properties 0 end nil thing)
     (--extension-completion (when (stringp thing) thing))))
  (prog1
      (switch-to-buffer (find-file-noselect
			 (concat "~/.emacs.d/extensions/" extension ".el")))))

(defun get-extension-documentation (extension)
  "Display the full documentation for EXTENION (a string)."
  (interactive (--extension-completion))
  (let ((var (intern-soft (format "user-ext-%s-documentation" extension))))
    (print-expr sexp (list var extension))
    (unless var
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
(load-extension "custom-ext")

(load-extension "general")
(load-extension "macro-ext")

(load-extension-safe "lsp-ext")
(load-extension "dired-ext")
(load-extension "buffers-ext")
(load-extension "imenu-ext")
(load-extension "syntax-ext")

(load-extension "keymaps-ext")
(load-extension-safe "yasnippets-ext")
(load-extension-safe "liquidsoap-bootstrap")
