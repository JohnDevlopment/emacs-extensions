;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(require 'menu-bar)

(eval-when-compile
  (declare-function help-ext-command "help-ext")
  (defvar help-ext-command))


;; ### Keymaps

;; --- Subject lookup
(define-prefix-command 'help-ext-command-map)
(keymaps-ext-set-keymap-global "C-h M-c" help-ext-command-map)
(keymaps-ext-set-keymap help-ext-command-map "l" #'elisp-index-search)
(keymaps-ext-set-keymap help-ext-command-map "u" #'emacs-index-search)


;; --- Apropos
(define-prefix-command 'help-ext-apropos-map)
(keymaps-ext-set-keymap-global "C-h A" #'help-ext-apropos-map)
(keymaps-ext-set-keymap help-ext-apropos-map "o" #'apropos)
(keymaps-ext-set-keymap help-ext-apropos-map "l" #'apropos-library)
(keymaps-ext-set-keymap help-ext-apropos-map "v" #'apropos-variable)


;; ### Hook

(defun help-ext--search (regexp &optional _x)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regexp (line-end-position) t)))

;;;###autoload
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

;;;###autoload
(add-hook 'help-mode-hook #'help--extra-hook)


(extension-provide 'help-ext)
;;; help-ext.el ends here
