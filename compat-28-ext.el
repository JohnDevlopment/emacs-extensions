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

(compat-defun replace-regexp-in-region (regexp replacement &optional start end)
  "Replace REGEXP with REPLACEMENT in the region from START to END.
The number of replaced occurrences are returned, or nil if REGEXP
doesn't exist in the region.

If START is nil, use the current point.  If END is nil, use `point-max'.

Comparisons and replacements are done with fixed case.

REPLACEMENT can use the following special elements:

  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\?' is treated literally."
  (if start
      (when (< start (point-min))
        (error "Start before start of buffer"))
    (setq start (point)))
  (if end
      (when (> end (point-max))
        (error "End after end of buffer"))
    (setq end (point-max)))
  (save-excursion
    (goto-char start)
    (save-restriction
      (narrow-to-region start end)
      (let ((matches 0)
            (case-fold-search nil))
        (while (re-search-forward regexp nil t)
          (replace-match replacement t)
          (setq matches (1+ matches)))
        (and (not (zerop matches))
             matches)))))

(extension-provide 'compat-28-ext)
;;; compat-28-ext.el ends here
