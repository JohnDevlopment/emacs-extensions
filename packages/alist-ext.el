;;; alist-ext.el --- Functions for alists            -*- lexical-binding: t; -*-

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

(require 'debug-ext)
(require 'cl-lib)

(defun alist-ext-from-list (list)
  "Turn a regular list LIST into an alist.
LIST must have an even number of elements."
  (unless (= (mod (length list) 2) 0)
    (user-error "Must have even number of arguments."))
  (seq-partition list 2))

(defmacro alist-ext-define (&rest pairs)
  "Construct an alist with PAIRS, where each key precedes its value.

Each key can be any valid lisp object, but symbols have to
be quoted.

\(fn KEY VALUE ...)"
  (unless (= (mod (length pairs) 2) 0)
    (error "alist-ext-define requires an even number of arguments (key-value pairs)."))
  (let (res key value)
    (while pairs
      (setq key (pop pairs) value (pop pairs))
      (cl-append-list (list 'cons key value) res))
    `(list ,@res)))

(provide 'alist-ext)

;;; alist-ext.el ends here
