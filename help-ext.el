;; -*- lexical-binding: t; -*-

(require 'menu-bar)

(eval-when-compile
  (declare-function help-ext-command "help-ext")
  (defvar help-ext-command))

(document-extension "help-ext"
  "Add keybindings to the help commands.

\\<help-ext-command>")

(define-prefix-command 'help-ext-command)
(global-set-key (kbd "C-h M") #'help-ext-command)

;; Subject lookup
(define-key help-ext-command (kbd "l") #'elisp-index-search)
(define-key help-ext-command (kbd "u") #'emacs-index-search)

(provide 'help-ext)

;;; help-ext.el ends here
