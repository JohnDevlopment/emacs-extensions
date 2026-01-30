;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")

(eval-when-compile
  (require 'function-ext))

;; (fext-defadvice flycheck-mode (after flycheck-mode)
;;   (when (bound-and-true-p flymake-mode)
;;     (flymake-mode 0)))

;;;###autoload
(defun flycheck--extra-hook () t)

;;;###autoload
(add-hook 'flycheck-mode-hook #'flycheck--extra-hook)

(extension-provide 'flycheck-ext)
;;; flycheck-ext.el ends here
