;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(autoload 'hi-lock-read-face-name "hi-lock" nil t)

;;;###autoload
(defun text-ext-highlight-even-lines ()
  "Highlight every even-numbered line in the current buffer."
  (interactive)
  (let ((overlay nil)
        (face '(:background "light blue")))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-to-number (format-mode-line "%l"))))
          (when (cl-evenp line)
	    (add-text-properties (line-beginning-position)
				 (line-end-position)
				 (list 'face face)))
          (forward-line 1))))))

;;;###autoload
(defun text-ext-lineify (start end)
  "Break text in region START and END into lines."
  (interactive (let ((start (and (use-region-p) (region-beginning)))
		     (end (and (use-region-p) (region-end))))
		 (if (use-region-p)
		     (list start end)
		   (list (point-min) (point-max)))))
  (cl-flet ((dsp ()
		 (let* ((pm (point-marker))
			(dist (save-excursion
				(skip-syntax-forward "-"))))
		   (delete-region pm (+ pm dist))
		   (goto-char pm))))
    (goto-char start)
    (setq start (point-marker)
	  end (save-excursion
		(goto-char end)
		(point-marker)))
    (until (>= (point) end)
      (when (= (skip-syntax-forward "^.") 0)
	(goto-char end))
      (cond ((memql (char-after) '(?. ?! ??))
	     (if (y-or-n-p "Separate line from here? ")
		 (cl-ext-progn
		   (forward-char 1)
		   (dsp)
		   (newline-and-indent))
	       (forward-char 1)))
	    ((memql (char-after) '(?, ?\; ?:))
	     (forward-char 1))))))

(provide 'text-ext)

;;; text-ext ends here
