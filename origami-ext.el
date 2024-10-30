;;; origami-ext --- Origami mode extension.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'origami))

;;;###autoload
(defun origami--extra-hook ())

;;;###autoload
(add-hook 'origami-mode-hook #'origami--extra-hook)

(define-key origami-mode-map (kbd "C-c f t")     #'origami-toggle-node)
(define-key origami-mode-map (kbd "C-c f o")     #'origami-open-node)
(define-key origami-mode-map (kbd "C-c f M-o")   #'origami-open-node-recursively)
(define-key origami-mode-map (kbd "C-c f C-o")   #'origami-show-only-node)
(define-key origami-mode-map (kbd "C-c f c")     #'origami-close-node)
(define-key origami-mode-map (kbd "C-c f M-c")   #'origami-close-node-recursively)
(define-key origami-mode-map (kbd "C-c f C-M-o") #'origami-open-all-nodes)

(provide 'origami-ext)

;;; origami-ext ends here
