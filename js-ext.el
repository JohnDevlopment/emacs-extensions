;;; js-ext --- Javascript mode extension.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defgroup user-ext-js2-mode nil
  "A group for `js2-mode'."
  :group 'user-extensions)

(defcustom user-ext-js-indent-tab-mode nil
  "Controls the local value of `indent-tabs-mode'."
  :type '(choice (const :tag "Tabs" t)
		 (const :tag "Spaces" nil)))

(defcustom user-ext-js-indent-spaces 4
  "The number of spaces that comprise a tab."
  :group 'user-ext-js2-mode
  :type 'integer)

;;;###autoload
(defun js2--extra-hook ()
  (setq indent-tabs-mode user-ext-js-indent-tab-mode)
  (setq tab-width user-ext-js-indent-spaces)
  (setq-local standard-indent 4))

;;;###autoload
(add-hook 'js2-mode-hook #'js2--extra-hook)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'js-ext)

;;; js-ext ends here
