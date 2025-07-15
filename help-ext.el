;; -*- lexical-binding: t; -*-

(require 'menu-bar)

(eval-when-compile
  (require 'cl-ext)

  (declare-function help-ext-command "help-ext")
  (defvar help-ext-command))

(eval-and-compile
  (define-prefix-command 'help-ext-command)
  (global-set-key (kbd "C-h M") #'help-ext-command)

  ;; Subject lookup
  (define-key help-ext-command (kbd "l") #'elisp-index-search)
  (define-key help-ext-command (kbd "u") #'emacs-index-search)

  (define-prefix-command 'help-ext-apropos-map)
  (global-set-key (kbd "C-h A") #'help-ext-apropos-map)
  (define-key help-ext-apropos-map (kbd "o") #'apropos)
  (define-key help-ext-apropos-map (kbd "l") #'apropos-library)
  (define-key help-ext-apropos-map (kbd "v") #'apropos-variable))

;; Hook

(defun help-ext--search (regexp &optional _x)
  (cl-ext-save-point
    (goto-char (point-min))
    (re-search-forward regexp (line-end-position) t)))

(defun help--extra-hook ()
  (run-with-idle-timer
   0.1 nil
   (lambda ()
     (with-current-buffer (help-buffer)
       (visual-line-mode)
       (cond ((help-ext--search (rx (or "’s value is" "is a variable")))
	      (message "yes")
	      (font-lock-debug-fontify))
	     ((help-ext--search "is a type")
	      (if visual-line-mode (visual-line-mode 0))
	      (setq word-wrap nil)
	      (toggle-truncate-lines 1)))))))

(--ignore
 (cl-prettyprint (symbol-function 'help--extra-hook))
 t)

(add-hook 'help-mode-hook #'help--extra-hook)

(provide 'help-ext)

;;; help-ext.el ends here
