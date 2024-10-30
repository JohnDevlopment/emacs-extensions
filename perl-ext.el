;;; perl-ext --- Perl mode extension.

;;; Commentary:

;;; Code:

(require 'perl-mode)

;;;###autoload
(defun perl-mode--extra-hook ())

;;;###autoload (autoload 'perl-ext-scratch-buffer "perl-ext" "Create a Perl scratch buffer." t)
(define-scratch-buffer-function perl-ext-scratch-buffer "perl scratch" nil
  "Create a Perl scratch buffer."
  nil
  (perl-mode))

;; M-SPC to do completion
(define-key perl-mode-map (kbd "M-SPC") #'completion-at-point)

;;;###autoload
(add-hook 'perl-mode-hook #'perl-mode--extra-hook)

(provide 'perl-ext)

;;; perl-ext ends here
