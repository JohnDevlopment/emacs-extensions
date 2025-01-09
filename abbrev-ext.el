;; -*- lexical-binding: t; -*-

(defun abbrev-ext-insert-hook ()
  t)

(put #'abbrev-ext-insert-hook 'no-self-insert t)

(advice-add 'add-mode-abbrev :after #'deactivate-mark)
(advice-add 'add-global-abbrev :after #'deactivate-mark)

(provide 'abbrev-ext)
