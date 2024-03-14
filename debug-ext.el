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

(defmacro assert (form &optional message)
  (declare (indent 2))
  "Assert that FORM evaluates to non-nil.
Otherwise throw an error."
  `(unless ,form
     ,(if message
	  `(user-error ,message)
	`(user-error (format "Assertion failed: %s" ',form)))))

(defun debug-ext-get-function-body (symbol)
  "Get the function definition of SYMBOL."
  (indirect-function symbol))

(provide 'debug-ext)

;;; debug-ext.el ends here
