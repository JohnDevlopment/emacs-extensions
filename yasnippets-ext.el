(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/dropdown-list")
  (require 'dropdown-list))

(add-to-list 'completion-at-point-functions #'yasnippet-capf)

(defun yas-ext-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))
