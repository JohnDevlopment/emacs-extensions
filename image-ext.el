;; -*- lexical-binding: t; -*-

;;;###autoload
(defun image--extra-hook ()
  t)

;;;###autoload
(add-hook 'image-mode-hook #'image--extra-hook)

(define-key image-mode-map (kbd "k") #'kill-and-quit)

(provide 'image-ext)
;;; image-ext.el ends here
