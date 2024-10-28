;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(defun text-ext-highlight-even-lines ()
  "Highlight every even-numbered line in the current buffer."
  (interactive)
  (let ((overlay nil)
        (face '(:background "light blue")))
    ;; Remove previous overlays
    ;; (remove-overlays (point-min) (point-max) 'highlight-even-lines t)
    ;; Iterate through each line in the buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-to-number (format-mode-line "%l"))))
          (when (cl-evenp line)
            ;; (setq overlay (make-overlay (line-beginning-position)
	    ;; 				(line-end-position)))
            ;; (overlay-put overlay 'face face)
            ;; (overlay-put overlay 'highlight-even-lines t))
	    (add-text-properties (line-beginning-position)
				 (line-end-position)
				 (list 'face face)))
        (forward-line 1))))))

;; (defun text-ext--evenp (n)
;;   "Return t if N is an even number."
;;   (zerop (% n 2)))

;; (global-set-key (kbd "C-c e") 'highlight-even-lines)
