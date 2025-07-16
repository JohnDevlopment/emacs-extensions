;; -*- lexical-binding: t; -*-

(require 'table)

;; ### Minor mode for inside a table

;;;###autoload
(define-minor-mode table-mode
  "Minor mode for tables created via `table-insert'."
  :lighter " Table"
  :keymap (let ((map (copy-keymap table-cell-map)))
	    map))

(provide 'table-ext)
;;; table-ext.el ends here
