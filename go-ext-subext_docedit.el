;; -*- lexical-binding: t; -*-

;; ### Customization

(defgroup go-ext-docedit nil
  "Group for Go docedit."
  :group 'go-ext)

(defface go-ext-docedit-edit-overlay-face
  '((((min-colors 88) (background dark))
     (:background "yellow1" :foreground "black"))
    (((background dark)) (:background "yellow" :foreground "black"))
    (((min-colors 88)) (:background "yellow1"))
    (t (:background "yellow")))
  "Face for `go-ext-docedit-overlay-category'."
  :group 'go-ext-docedit)


;; ### Variables

(defconst go-ext-docedit-overlay-category
  (eval-and-compile
    (let ((cat 'go-ext-overlay-edit))
      (setplist cat '(face go-ext-docedit-edit-overlay-face start cursor-intangible t))
      cat)))

(defconst go-ext-docedit-buffer "*go docedit*")

(defvar-local go-ext-docedit--type nil)

(defvar-local go-ext-docedit--indentation nil)

(defvar-local go-ext-docedit--start nil)

(defvar-local go-ext-docedit--end nil)
(make-obsolete-variable 'go-ext-docedit--end nil "2025-09-17")

(defvar-local go-ext-docedit--source-buffer nil)


;; ### Minor mode

(defconst go-ext-docedit-register ?g)

(defconst go-ext-docedit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'go-ext-docedit-apply)
    (define-key map [remap save-buffer] #'go-ext-docedit-save)
    map)
  "Keymap for docedit minor mode.")

(define-minor-mode go-ext-docedit-mode
  "Text scratch mode."
  :group 'go-ext
  :lighter " Go Docedit"
  :keymap go-ext-docedit-mode-map
  (if go-ext-docedit-mode
      (cl-ext-progn
	(auto-fill-mode 1)
	(display-fill-column-indicator-mode 1))
    (auto-fill-mode 0)
    (display-fill-column-indicator-mode 0)))

(defun go-ext-docedit--find-overlay (&optional pos)
  (cl-loop with p = (or pos (point))
	   for ov being the overlays from p
	   do
	   (when (eq (overlay-get ov 'category)
		     go-ext-docedit-overlay-category)
	     (cl-return ov))))

(defun go-ext-docedit-save ()
  ;; TODO: documentation string
  (interactive "*")
  (or (buffer-modified-p)
      (user-error "Nothing to save"))
  (let (string)
    (cl-declare (special p))
    (go-ext-docedit--validate-vars)
    (cl-ecase go-ext-docedit--type
      (?s (setq string (buffer-string-no-properties))
	  (let ((beg go-ext-docedit--start) end
		ov)
	    (with-current-buffer go-ext-docedit--source-buffer
	      (when-let ((ov (go-ext-docedit--find-overlay beg))
			 (start (overlay-start ov))
			 (end (overlay-end ov)))
		(delete-overlay ov)
		(delete-region start end))
	      (goto-char beg)
	      (princ string (current-buffer))
	      (setq end (point-marker)
		    ov (make-overlay beg end))
	      (overlay-put ov 'category go-ext-docedit-overlay-category)))
	  (set-buffer-modified-p nil))
      (?c (setq string (buffer-string-no-properties))
	  (with-temp-buffer
	    ;; Add "// " to the start of each line, then get the string
	    (save-excursion (princ string (current-buffer)))
	    (until (eobp)
	      (beginning-of-line)
	      (insert "// ")
	      (forward-line 1))
	    (setq string (buffer-string-no-properties)))
	  (let ((beg go-ext-docedit--start) end p ov)
	    (cl-assert beg)
	    (with-current-buffer go-ext-docedit--source-buffer
	      (when-let ((ov (go-ext-docedit--find-overlay beg))
			 (start (overlay-start ov))
			 (end (overlay-end ov)))
		;; Delete the "edit" overlay
		(delete-overlay ov)
		(delete-region start end))
	      (setq p (cl-ext-progn
			(goto-char beg)
			(point-marker))
		    end (cl-ext-progn
			  (princ string (current-buffer))
			  (point-marker))
		    ov (make-overlay beg end))
	      (overlay-put ov 'category go-ext-docedit-overlay-category)
	      (indent-region beg end))
	    (set-buffer-modified-p nil))))))

(defun go-ext-docedit-apply ()
  "\"Apply\" the contents of the buffer and close the buffer."
  (interactive)
  (let ((pos go-ext-docedit--start)
	string)
    (cl-ecase go-ext-docedit--type
      (?s (setq string (buffer-string-no-properties))
	  (kill-region (point-min) (point-max))
	  (kill-buffer)
	  (jump-to-register go-ext-docedit-register)
	  (when-let ((ov (go-ext-docedit--find-overlay pos))
		     (start (overlay-start ov))
		     (end (overlay-end ov)))
	    (delete-overlay ov)
	    (delete-region start end))
	  (princ string (current-buffer)))
      (?c (goto-char (point-min))
	  (until (eobp)
	    (insert "// ")
	    (forward-line 1))
	  (setq string (buffer-string-no-properties))
	  (let (p)
	    (kill-buffer)
	    (jump-to-register go-ext-docedit-register)
	    (when-let ((ov (go-ext-docedit--find-overlay pos))
		       (start (overlay-start ov))
		       (end (overlay-end ov)))
	      (delete-overlay ov)
	      (delete-region start end))
	    (setq p (point-marker))
	    (princ string (current-buffer))
	    (indent-region p (point)))))))

(defun go-ext-docedit--validate-vars ()
  (cl-macrolet ((assert-non-nil
		 (symbol)
		 `(or ,symbol
		      (error ,(format "`%S' is nil" symbol)))))
    (assert-non-nil go-ext-docedit--type)
    (cl-check-type go-ext-docedit--type character)
    (assert-non-nil go-ext-docedit--start)
    (cl-check-type go-ext-docedit--start marker)
    ;; (assert-non-nil go-ext-docedit--end)
    ;; (cl-check-type go-ext-docedit--end marker)
    (assert-non-nil go-ext-docedit--source-buffer)
    (cl-check-type go-ext-docedit--source-buffer buffer)))
(cl-define-compiler-macro go-ext-docedit--validate-vars
    (&whole form)
  (when (or (not (emacs-version-cond-when-compile
		   ((>= "28.1")
		    (macroexp-compiling-p))
		   (t (cl--compiling-file))))
	    (< cl--optimize-speed 3) (= cl--optimize-safety 3))
    form))

(defun go-ext-docedit ()
  "Edit the string at point."
  (interactive)
  (let ((old-buffer (current-buffer))
	bounds string beg end)
    (cond
     ;; Inside a line comment
     ((save-excursion
	(end-of-line)
	(eq (go-in-comment-p) t))
      (let ((indent (current-indentation)))
	(beginning-of-line)
	(setq beg (cl-ext-progn
		    ;; Go up line by line until a non-comment line,
		    ;; then go to the beginning of the first comment line
		    (while (and (not (bobp))
				(looking-at-p "^\\s-*//"))
		      (forward-line -1))
		    (unless (bobp)
		      (forward-line 1))
		    (point-marker))
	      end (cl-ext-progn
		    ;; Find the end point of the comment lines
		    (while (looking-at-p "^\\s-*//")
		      (forward-line 1))
		    (forward-line -1)
		    (end-of-line)
		    (point-marker))
	      string (buffer-substring-no-properties beg end))
	(window-configuration-to-register go-ext-docedit-register)
	(condition-case err
	    (cl-ext-progn
	      (and (> end beg) (kill-region beg end))
	      (go-ext-docedit--buffer)
	      (setq go-ext-docedit--type ?c
		    go-ext-docedit--indentation indent
		    go-ext-docedit--source-buffer old-buffer
		    go-ext-docedit--start beg
		    ;; go-ext-docedit--end end
		    )
	      (princ string (current-buffer))
	      (go-ext-docedit--validate-vars)
	      (goto-char (point-min))
	      (save-excursion
		(until (eobp)
		  (when (looking-at "^[ \t]*//[ \t]*\\(.*\\)")
		    (replace-match "\\1"))
		  (forward-line 1))))
	  (error (message "`go-ext-docedit' error: %S" err)
		 (go-ext-kill-buffer go-ext-docedit-buffer)
		 (jump-to-register go-ext-docedit-register)))))
     ;; Inside a string
     ((and (setq bounds (thing-at-point-ext-bounds-of-string-at-point)
		 beg (let ((it (car-safe bounds)))
		       (and it (1+ it)))
		 end (let ((it (cdr bounds)))
		       (and it (1- it)))
		 string (if (and beg end (> end beg))
			    (buffer-substring-no-properties beg end)
			  ""))
	   beg end string)
      (window-configuration-to-register go-ext-docedit-register)
      (condition-case err
	  (cl-ext-progn
	    (and (> end beg) (kill-region beg end))
	    (go-ext-docedit--buffer)
	    (let ((beg beg)
		  (end end))
	      (with-current-buffer old-buffer
		(setq beg (save-excursion
			    (goto-char beg)
			    (point-marker))
		      end (save-excursion
			    (goto-char end)
			    (point-marker))))
	      (setq go-ext-docedit--source-buffer old-buffer
		    go-ext-docedit--type ?s
		    go-ext-docedit--start beg
		    ;; go-ext-docedit--end end
		    ))
	    (princ string (current-buffer))
	    (go-ext-docedit--validate-vars))
	(error (message "`go-ext-docedit' error: %S" err)
	       (go-ext-kill-buffer go-ext-docedit-buffer)
	       (jump-to-register go-ext-docedit-register)
	       (princ string old-buffer))))
     (t (user-error "Cannot do anything here")))))

(define-scratch-buffer-function go-ext-docedit--buffer "go docedit" nil
  "Buffer for `go-ext-docedit'."
  nil
  (text-mode)
  (go-ext-docedit-mode 1))


(cl-pushnew 'docedit user-ext-go-subextensions)
;;; docedit.el ends here
