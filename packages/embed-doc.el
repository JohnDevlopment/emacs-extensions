;;; embed-doc.el --- Embed documentation in libraries  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  John

;; Author: John <john@john-System-Product-Name>
;; Keywords: docs, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (declare-function company-capf "company-capf" (command &optional arg &rest rest)))

;; Variables

(defconst embed-doc-prop
  'embedded-symbol-documentation)

(defconst embed-doc-keywords
  '(:commands :customs :faces :functions :variables)
  "Valid keywords for `embed-doc-document-library'")

;; Functions

(defun embed-doc-error (string &rest args)
  (let ((string (apply #'format string args)))
    (error "embed-doc: %s" string)))

(defun embed-doc--split-list (pred xs)
  (let ((ys (list nil)) (zs (list nil)) flip)
    (cl-dolist (x xs)
      (if flip
          (nconc zs (list x))
        (if (funcall pred x)
            (progn
              (setq flip t)
              (nconc zs (list x)))
          (nconc ys (list x)))))
    (cons (cdr ys) (cdr zs))))

(defun embed-doc--normalize-plist (name input &optional plist)
  "Turn pseudo PLIST into a normal plist."
  (if (null input)
      plist
    (let* ((keyword (car input))
	   (xs (embed-doc--split-list
		#'keywordp (cdr input))) ; -> ((KW-ARGS...) OTHER-ARGS...)
	   (args (car xs))		 ; -> (KW-ARGS...)
	   (tail (cdr xs))		 ; -> (OTHER-ARGS...)
	   (error-string (format "Unrecognized keyword: %S" keyword)))
      (unless (memq keyword embed-doc-keywords)
	(embed-doc-error error-string))
      (setq plist (embed-doc--normalize-plist name tail plist))
      (plist-put plist keyword args))))

(defun embed-doc--string-section (doc heading symbols)
  (format "%s\n\n%s\n%s" doc heading
	  (mapconcat (lambda (x) (format "- `%S'" x)) symbols "\n")))

;;;###autoload
(defsubst embed-doc-get-documentation (symbol)
  (cl-check-type symbol symbol)
  (let ((doc (get symbol embed-doc-prop)))
    (when doc
      (substitute-command-keys doc))))

;;;###autoload
(defmacro embed-doc-document-symbol (symbol preamble &rest args)
  "Embed documentation in SYMBOL, starting with PREAMBLE.
PREAMBLE is the first part of the documentation.

  (embed-doc-document-symbol symbol
    [:keyword [option]]...)

:commands    Document commands (i.e., interactive functions).
:customs     Document user options (i.e., variables defined
             with `defcustom').
:faces       Document faces (i.e., defined with `defface').
:functions   Document functions.
:variables   Document variables.

It does not replace any existing documentation, so functions,
variables, and faces.

\(fn SYMBOL PREAMBLE ARG...)"
  (declare (debug (symbol stringp
			  [&optional ":commands" &rest symbolp]
			  [&optional ":customs" &rest symbolp]
			  [&optional ":faces" &rest symbolp]
			  [&optional ":functions" &rest symbolp]
			  [&optional ":variables" &rest symbolp]))
	   (indent 1))
  (cl-check-type symbol symbol)
  (cl-check-type preamble string)
  (let ((args (embed-doc--normalize-plist symbol args))
	(doc (format "Documentation for extension %S:

%s" symbol preamble))
	keyword kwargs)
    (while args
      (setq keyword (pop args) kwargs (pop args))
      (when kwargs
	(pcase keyword
	  (:commands
	   (setq doc (embed-doc--string-section doc "Commands:" kwargs)))
	  (:customs
	   (setq doc (embed-doc--string-section doc "User Options:" kwargs)))
	  (:faces
	   (setq doc (embed-doc--string-section doc "Faces:" kwargs)))
	  (:functions
	   (setq doc (embed-doc--string-section doc "Functions:" kwargs)))
	  (:variables
	   (setq doc (embed-doc--string-section doc "Variables:" kwargs))))))
    `(prog1 t
       (put ',symbol ',embed-doc-prop ,doc))))

(provide 'embed-doc)
;;; embed-doc.el ends here
