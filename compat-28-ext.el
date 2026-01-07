;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(eval-when-compile
  (load "compat-macs.el"))

(compat-version "28.1")

(compat-defun seq-union (&rest sequences)
  "Return a new list that is the union of the given SEQUENCES.
Duplicates are removed."
  (let ((result '()))
    (dolist (seq sequences)
      (dolist (item seq)
        (unless (member item result)
          (push item result))))
    (nreverse result)))

(extension-provide 'compat-28-ext)
;;; compat-28-ext.el ends here
