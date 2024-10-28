;;; toml-ext --- TOML mode extension.  -*- lexical-binding: t;  -*-

;;; Commentary:

;;; Code:

(require 'toml-mode)
(require 'json-mode)

;;; Code:

(defun toml-ext-indent ()
  "Indent function for `toml-mode'."
  (interactive)
  (let* ((parse-status
	  (save-excursion (syntax-ppss (point-at-bol))))
	 (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)	; unless point is inside a string
      (indent-line-to (js--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

;;;###autoload
(defun toml-mode--extra-hook ()
  "Extra hook for `toml-mode'."
  (setq-local indent-line-function #'toml-ext-indent))

;;;###autoload
(add-hook 'toml-mode-hook #'toml-mode--extra-hook)

(provide 'toml-ext)

;;; toml-ext ends here
