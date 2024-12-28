;;; cl-ext.el --- Common lisp extensions extension.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John

;; Author: John <john@john-System-Product-Name>
;; Keywords: extensions

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

(defmacro cl-append (x place)
  "Add X to the list stored in PLACE.
PLACE is a symbol whose definition is a list or some other
kind of sequence."
  (declare (pure t) (debug (form symbolp)))
  (cl-check-type place symbol)
  `(setq ,place (append ,place ,x)))

(defalias 'cl-pushend 'cl-append)
(make-obsolete 'cl-pushend 'cl-append "2024.12.21")

(defmacro cl-append-list (x place)
  "Add X to the list stored in PLACE, but wrap X in a list.
X is supposed to be a list, likely representing a Lisp
expression; it's added to PLACE in such a way that it isn't
destructured (see `append').

Let's use an example.  Supposed you're building a list of
Lisp expressions and adding another list to it:
   (let ((body '(\"Docstring.\")))
     (cl-append-list (interactive) body))
   => (\"Docstring\" (interactive))"
  (declare (pure t) (debug (form symbolp)))
  (cl-check-type x listp)
  (cl-check-type place symbol)
  `(setq ,place (append ,place (list ,x))))

(defmacro cl-ext-nconcat (place &rest sequences)
  "Append the arguments (SEQUENCES) as strings to PLACE.
the string found at PLACE and SEQUENCES are combined via
`concat' and then set as the new value of PLACE."
  `(setq ,place (concat ,place ,@sequences)))

(defmacro cl-save-point (&rest body)
  "Execute BODY and restore point to its original position.
Any errors are caught and printed as simple messages.

\(fn BODY...)"
  (declare (indent 0))
  `(let ((cl--point (point-marker))
	 cl--result)
     (setq cl--result (with-demoted-errors "Error caught from cl-save-point: %S"
			,@body))
     (goto-char cl--point)
     cl--result))

(provide 'cl-ext)

;;; cl-ext.el ends here
