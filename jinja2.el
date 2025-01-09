;;; jinja2.el --- Jinja2 minor mode                  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John

;; Author: John
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

(jinja2-define-skeleton function
  "Insert a function call."
  "Function: "
  "{% " str
  "(" ("Argument, %s: "
       (unless (= (char-before) ?\() ", ")
       str)
  resume:
  ") %}")

(jinja2-define-skeleton set
  "Insert a variable assignment."
  "Name: "
  '(setq v1 (if arg "{%-" "{%"))
  '(setq v2 (if arg "-%}" "%}"))
  v1 " set " str " = " _ ?\  v2)

(jinja2-define-skeleton block
  "Insert a block."
  "Name: "
  '(setq v1 (if arg "{%-" "{%"))
  '(setq v2 (if arg "-%}" "%}"))
  v1 " block " str ?\  v2 \n
  _ \n v1 " endblock " v2)

(jinja2-define-skeleton prompt-function
  "Insert a call to the global prompt function."
  nil
  "{{- prompt(\"" (progn
		   (setq v1 (read-string "Key: "))
		   (if (string-empty-p v1)
		       "<undefined key>"
		     v1))
  "\", "
  "\"" (progn
	 (setq v1 (read-string "Prompt: "))
	 (if (string-empty-p v1)
	     "<undefined prompt>"
	   v1))
  "\""
  (when (y-or-n-p "Add `type_'? ")
    ;; type_=...
    (setq v1 (read-string "Type: "))
    (unless (string-empty-p v1)
      (format ", type_=\"%s\"" v1)))
  (when (y-or-n-p "Add `default'? ")
    ;; default=...
    (setq v1 (read-string "Default (not quoted): "))
    (unless (string-empty-p v1)
      (format ", default=%s" v1)))
  ") -}}")

(jinja2-define-skeleton prompt-list-function
  "Insert a call to the global prompt-list function."
  "Key: "
  "{{- prompt_list(\""
  ") -}}")

(defun jinja2-insert-expression ()
  "Insert a variable expansion."
  (interactive)
  (insert "{{  }}")
  (left-char 3))

;; Functions

(defun jinja2--get-leading-whitespace ()
  (cl-save-point
    (beginning-of-line)
    (when (looking-at "\\([ \t]+\\)")
      (match-string 1))))

(defun jinja2-set-tag-trim (&optional arg)
  (interactive "P")
  (cond
   ((= (char-after) ?{)
    (right-char 3))
   ((= (char-before) ?})
    (left-char 3)))
  (let* ((ppss (make-ppss-easy (syntax-ppss)))
	 (pcstr "(choices: +, -, or \" \")") ; prompt choices string
	 (choices (list ?+ ?- ?\ ))
	 beg
	 end
	 c)
    ;; (message "%s"
    ;; 	     (with-output-to-string
    ;; 	       (print-expr sexp )))
    (cl-save-point
      (setq beg (progn (goto-char (syntax-ppss-toplevel-pos ppss))
		       (point-marker))
	    end (progn (goto-char beg)
		       (forward-sexp)
		       ;; (left-char 3)
		       (point-marker)))
      (goto-char beg)
      (when arg
	(setq c (read-char-from-minibuffer (s-lex-format
					    "lstrip_blocks and trim_blocks flags ${pcstr}: ")))
	(unless (cl-member c choices)
	  (user-error "Invalid choice %c: must be one of +, -, or \" \"" c)))
      (when (looking-at "{%[+-]?\\s-*")
	(unless arg
	  ;; No prefix arg; accept flag for this
	  (setq c (read-char-from-minibuffer
		   (s-lex-format "lstrip_blocks flag ${pcstr}: ")))
	  (unless (cl-member c choices)
	    (user-error "Invalid choice %c: must be one of +, -, or \" \"" c)))
	(cl-case c
	  ((?+ ?-)
	   (replace-match (format "{%%%c " c)))
	  (t
	   (replace-match "{% "))))
      (goto-char end)
      (when (looking-back "\\s-*[+-]?\\([}%]\\)}" beg t)
	(unless arg
	  ;; No prefix arg; accept flag for this
	  (setq c (read-char-from-minibuffer
		   (s-lex-format "trim_blocks flag ${pcstr}: ")))
	  (unless (cl-member c choices)
	    (user-error "Invalid choice %c: must be one of +, -, or \" \"" c)))
	(cl-case c
	  ((?+ ?-)
	   (replace-match (format " %c%%}" c)))
	  (t
	   (replace-match " %}")))))))

;; Keybinds

(defvar jinja2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd (concat jinja2-command-prefix " b")) #'jinja2-skeleton-block)
    (define-key map (kbd (concat jinja2-command-prefix " #")) #'jinja2-skeleton-comment)
    (define-key map (kbd (concat jinja2-command-prefix " f")) #'jinja2-skeleton-for)
    (define-key map (kbd (concat jinja2-command-prefix " F")) #'jinja2-skeleton-function)
    (define-key map (kbd (concat jinja2-command-prefix " i")) #'jinja2-skeleton-if)
    (define-key map (kbd (concat jinja2-command-prefix " m")) #'jinja2-skeleton-macro)
    (define-key map (kbd (concat jinja2-command-prefix " M-p")) #'jinja2-skeleton-prompt-function)
    (define-key map (kbd (concat jinja2-command-prefix " M-l")) #'jinja2-skeleton-prompt-list-function)
    (define-key map (kbd (concat jinja2-command-prefix " t")) #'jinja2-skeleton-tag)
    (define-key map (kbd (concat jinja2-command-prefix " s")) #'jinja2-skeleton-set)
    (define-key map (kbd (concat jinja2-command-prefix " e")) #'jinja2-insert-expression)
    (define-key map (kbd (concat jinja2-command-prefix " C-t")) #'jinja2-set-tag-trim)
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
