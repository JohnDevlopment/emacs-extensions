;; -*- lexical-binding: t; -*-


;; ### Functions

;;;###autoload
(defun hs-ext-hide-range
    (start end kind &optional b-offset e-offset no-hooks)
  "Hide the text between START and END.
START and END are inclusive.
KIND is either `code' or `comment'.
Unless NO-HOOKS is non-nil, run `hs-hide-hook' after hiding
the text.

See also: `hs-make-overlay'."
  (let* ((kind (pcase kind
		 ((and k (or 'code 'comment)) k)
		 ('t 'code)))
	 (ov (hs-make-overlay start end kind b-offset e-offset)))
    (hs-ext-add-keymap-to-overlay ov)
    (goto-char start)
    (unless no-hooks
      (run-hooks 'hs-hide-hook))))

;;;###autoload
(defun hs-ext-find-next-overlay (&optional pos)
  (cl-loop with pos = (or pos (point))
	   for ov being the overlays from pos
	   when (member (overlay-get ov 'hs) '(code comment))
	   return ov))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'hs-ext-find-next-overlay))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t)
   (call-interactively #'menu-bar--toggle-truncate-long-lines))
 t)

;;;###autoload
(defun hs-ext-add-keymap-to-overlay (ov)
  "Add a custom keymap to overlay OV."
  (overlay-put
   ov 'keymap
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<return>") #'hs-show-block)
     map)))

(fext-defadvice hs-hide-block-at-point
    (override hs-hide-block-at-point (&optional end comment-reg))
  (if comment-reg
      (hs-hide-comment-region
       (car comment-reg) (cadr comment-reg) end)
    (when (hs-looking-at-block-start-p)
      (let ((mdata (match-data t))
            (header-end (match-end 0))
            p q ov)
	;; `p' is the point at the end of the block beginning, which
	;; may need to be adjusted
	(save-excursion
	  (goto-char (funcall (or hs-adjust-block-beginning #'identity)
			      header-end))
	  (setq p (line-end-position)))
	;; `q' is the point at the end of the block
	(hs-forward-sexp mdata 1)
	(setq q (if (looking-back hs-block-end-regexp nil)
		    (match-beginning 0)
		  (point)))
        (when (and (< p q) (> (count-lines p q) 1))
          (cl-ext-cond
	    ((and hs-allow-nesting
		  (setq ov (hs-overlay-at p)))
             (delete-overlay ov))
            ((not hs-allow-nesting)
             (hs-discard-overlays p q)))
          (hs-ext-hide-range p q 'code (- header-end p)))
        (goto-char (if end q (min p header-end)))))))


;; ### Hooks

;;;###autoload
(defun hs--extra-hook () t)

;;;###autoload
(add-hook 'hs-minor-mode-hook #'hs--extra-hook)


(provide 'hs-ext)
;;; hs-ext.el ends here
