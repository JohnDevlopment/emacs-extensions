;; -*- lexical-binding: t; -*-

(require 'ibuffer)

(eval-when-compile
  (require 'alist-ext)
  (require 'function-ext))

;; Advice

(advice-add #'ibuffer-do-revert :after #'ibuffer-ext--after-operation)

(fext-defadvice ibuffer-do-view (after ibuffer-do-view)
  (view-mode 1))

(fext-defadvice ibuffer-visit-buffer (after ibuffer-visit-buffer)
  "Bury the IBuffer buffer after visiting the selected buffer."
  (bury-buffer (get-buffer "*Ibuffer*")))

(define-key ibuffer-mode-map (kbd "/ T") #'ibuffer-ext-toggle-current-filter-group)

;; Functions

(defun ibuffer-ext--after-operation (&rest _r)
  "Used as :after advice for operations."
  (ibuffer-unmark-all-marks))

;;;###autoload
(defun ibuffer-ext-toggle-current-filter-group ()
  "Expand/collapse the filter group of point.
If point is on a buffer, toggle the group it is in."
  (interactive)
  (unless (char-equal (char-after) ?\[)
    (ibuffer-backward-filter-group))
  (ibuffer-toggle-filter-group))

;;;###autoload
(defun ibuffer--extra-hook () t)

;;;###autoload
(add-hook 'ibuffer-mode-hook #'ibuffer--extra-hook)

(provide 'ibuffer-ext)

;;; ibuffer-ext.el ends here
