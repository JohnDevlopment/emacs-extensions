;;; jinja2.el --- Jinja2 minor mode                  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'cl-ext)
(require 'debug-ext)

;; Customize

(defgroup jinja2 nil
  "Group for `jinja2-mode'."
  :group 'tools)

(defcustom jinja2-lighter " Jinja2"
  "Mode line."
  :group 'jinja2
  :type 'string)

(defcustom jinja2-command-prefix "C-c j"
  "Command prefix for Jinja2."
  :type 'string
  :group 'jinja2)

;; Skeletons

(defmacro jinja2-define-auxiliary-skeleton (name &optional doc &rest skel)
  (declare (indent 1) (doc-string 2))
  (assert (symbolp name))
  (let* ((function-name (intern (format "jinja2--skeleton-%S" name)))
	 (msg (format "Insert %s clause? " name)))
    `(progn
       (define-skeleton ,function-name
	 ,(or doc (format "Insert %s clause." name))
	 nil
	 (unless (y-or-n-p ,msg)
	   (signal 'quit t))
	 ,@skel))))

(defmacro jinja2-define-skeleton (name doc &rest skel)
  (declare (indent 1) (doc-string 2))
  (assert (symbolp name))
  (let* ((function-name (intern (format "jinja2-skeleton-%S" name))))
    `(progn
       (define-skeleton ,function-name
	 ,doc
	 ,@skel))))

(jinja2-define-auxiliary-skeleton else
  nil
  "{% else %}" \n \n \n -1)

(jinja2-define-skeleton if
  "Insert an if statement."
  "Condition: "
  "{% if " str " %}" \n _ \n
  ("Condition: "
   "{% elif " str " %}" \n \n)
  '(jinja2--skeleton-else)
  "{% endif %}")

(jinja2-define-skeleton for
  "Insert a for loop."
  "Variable: "
  "{% for " str " in " (setq v1 (read-string "Iterable: ")) " %}"
  \n _ \n "{% endfor %}")

(jinja2-define-skeleton macro
  "Insert a macro definition."
  "Name: "
  "{% macro " str ?\( ("Parameter, %s: "
		       (unless (equal ?\( (char-before)) ", ")
		       str)
  ") %}" \n
  _
  \n
  "{% endmacro %}")

(jinja2-define-skeleton comment
  "Insert a comment."
  nil
  "{# " _ \n "#}")

(jinja2-define-skeleton tag
  "Insert a tag."
  nil
  "{% " _ " %}")

;; Functions

(defun jinja2--get-leading-whitespace ()
  (cl-save-point
    (beginning-of-line)
    (when (looking-at "\\([ \t]+\\)")
      (match-string 1))))

;; Keybinds

(defvar jinja2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd (concat jinja2-command-prefix " i")) #'jinja2-skeleton-if)
    (define-key map (kbd (concat jinja2-command-prefix " f")) #'jinja2-skeleton-for)
    (define-key map (kbd (concat jinja2-command-prefix " m")) #'jinja2-skeleton-macro)
    (define-key map (kbd (concat jinja2-command-prefix " #")) #'jinja2-skeleton-comment)
    (define-key map (kbd (concat jinja2-command-prefix " t")) #'jinja2-skeleton-tag)
    map)
  "Mode map for Jinja2.")

;;;###autoload
(define-minor-mode jinja2-mode
  "Minor mode for editing Jinja2 templates within major modes.

\\{jinja2-mode-map}"
  :lighter jinja2-lighter
  :require 'jinja2
  :keymap jinja2-mode-map
  (setq-local skeleton-further-elements
	      '((abbrev-mode nil)
		(^ '(- (1+ (current-indentation))))
		(|| ''(progn
			(when (> (current-indentation) v1)
			  (message "%S" v1)
			  (just-one-space)
			  (backward-delete-char-untabify 1)
			  (insert (or v2 ""))))))))

(provide 'jinja2)

;;; jinja2.el ends here
