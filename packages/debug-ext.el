;;; debug-ext.el --- Debug extension                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John
;; Author: John <john@john-System-Product-Name>

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
  (require 'cl-lib))

(defalias 'assert #'cl-assert)
(make-obsolete 'assert #'cl-assert "2024-12-24")

(defmacro print-expr (type form)
  "Print the result of FORM.
TYPE is used to indicate how FORM should be handled.
Currently, it can be either symbol `var' or symbol `sexp'.

FORM should not be quoted."
  (pcase type
    ('var
     (cl-check-type form symbol)
     ;; `(message ,(format "DEBUG: %s = %S" (cl-prin1-to-string form) form))
     `(message "DEBUG: %s = %S" ,(cl-prin1-to-string form) ,form))
    ('sexp
     `(message ,(concat "DEBUG: " (cl-prin1-to-string form) " = %S") ,form))
    (_ (error "Unknown type %S" type))))

(defun debug-ext-get-function-body (symbol)
  "Get the function definition of SYMBOL."
  (indirect-function symbol))

(provide 'debug-ext)

;;; debug-ext.el ends here
