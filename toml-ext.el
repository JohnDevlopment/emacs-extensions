;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")
(extension-check-requires toml-mode json-mode)

(eval-when-compile
  (require 'toml-mode)
  (require 'json-mode))

(declare-function 'js--proper-indentation "js" (parse-status))


;; ### Functions

(defun toml-ext-indent ()
  "Indent function for `toml-mode'."
  (interactive)
  (let* ((parse-status
	  (save-excursion (syntax-ppss (point-at-bol))))
	 (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)	; unless point is inside a string
      (indent-line-to (js--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))


;; ### Hooks

;;;###autoload
(defun toml-mode--extra-hook ()
  "Extra hook for `toml-mode'."
  (setq-local indent-line-function #'toml-ext-indent))

;;;###autoload
(add-hook 'toml-mode-hook #'toml-mode--extra-hook)


(extension-provide 'toml-ext)
;;; toml-ext.el ends here
