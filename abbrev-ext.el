;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'abbrev)
  (require 'cl-ext))

(defconst user-ext-abbrev-insert-char-regex
  (rx (or "i.e." "e.g." "interactively"))
  "Regular expression that matches the expansion that allows")

(defun abbrev-ext-insert-hook ()
  (let ((last-char last-command-event)
	(limit (cl-ext-save-point
		 (beginning-of-line)
		 (point))))
    (save-match-data
      (looking-back user-ext-abbrev-insert-char-regex limit)
      (not (= last-char ?,)))))

(put #'abbrev-ext-insert-hook 'no-self-insert t)

(advice-add 'add-mode-abbrev :after #'deactivate-mark)
(advice-add 'add-global-abbrev :after #'deactivate-mark)

(define-abbrev global-abbrev-table "ie" "i.e." #'abbrev-ext-insert-hook :system t)
(define-abbrev global-abbrev-table "eg" "e.g." #'abbrev-ext-insert-hook :system t)
(define-abbrev global-abbrev-table "regex" "regular expression" #'abbrev-ext-insert-hook :system t)
(define-abbrev global-abbrev-table "regexs" "regular expressions" #'abbrev-ext-insert-hook :system t)
