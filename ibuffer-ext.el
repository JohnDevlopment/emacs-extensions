;;; ibuffer-ext.el --- Extension for IBuffer.        -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'ibuffer))

(require 'alist-ext)

;; Advice

(advice-add 'ibuffer-do-revert :after #'ibuffer--after-operation
	    (alist-ext-define 'name "after-revert"))

(advice-add 'ibuffer-do-view :after
	    (lambda (&rest args)
	      (interactive)
	      (view-mode))
	    (alist-ext-define 'name "after-view"))

(define-key ibuffer-mode-map (kbd "/ T") #'ibuffer-toggle-current-filter-group)

;; FUnctions

(defun ibuffer--after-operation ()
  (ibuffer-unmark-all-marks))

;;;###autoload
(defun ibuffer-toggle-current-filter-group ()
  "Expand/collapse the filter group of point.
If point is on a buffer, toggle the group it is in."
  (interactive)
  (unless (char-equal (char-after) ?\[)
    (ibuffer-backward-filter-group))
  (ibuffer-toggle-filter-group))

;;;###autoload
(defun ibuffer--extra-hook ()
  (1+ 1))

;;;###autoload
(add-hook 'ibuffer-mode-hook #'ibuffer--extra-hook)

(provide 'ibuffer-ext)

;;; ibuffer-ext.el ends here
