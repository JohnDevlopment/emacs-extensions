;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'abbrev))

(defun abbrev-ext-insert-hook ()
  t)

(put #'abbrev-ext-insert-hook 'no-self-insert t)

(advice-add 'add-mode-abbrev :after #'deactivate-mark)
(advice-add 'add-global-abbrev :after #'deactivate-mark)

(define-abbrev global-abbrev-table "ie" "i.e." #'abbrev-ext-insert-hook :system t)
(define-abbrev global-abbrev-table "eg" "e.g." #'abbrev-ext-insert-hook :system t)
