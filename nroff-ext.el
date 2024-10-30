;;; nroff-ext --- Nroff mode extension.

;;; Commentary:

;;; Code:

(require 'nroff-mode)

;; Add 1p and 3p to nroff-mode auto mode alist
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[13]p\\'" . nroff-mode))

(provide 'nroff-ext)

;;; nroff-ext ends here
