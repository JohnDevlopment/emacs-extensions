;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(require 'ido)

(eval-when-compile
  (require 'function-ext)

  (use-package bookmark
    :functions
    bookmark-bmenu-this-window))

(defun bookmark-bmenu-ext-this-window-alternate ()
  "Select bookmark at point in the current window, and kill buffer.
Kill the bookmark list buffer unless it's modified (see `buffer-modified-p')."
  (interactive)
  (let ((curbuf (current-buffer)))
    (and (buffer-modified-p)
	 (bookmark-bmenu-save))
    (run-with-idle-timer 0.5 nil #'kill-buffer-if-not-modified curbuf)
    (bookmark-bmenu-this-window)))

(--ignore :no-warn
 (fext-replace-function bookmark-completing-read "bookmark-menu-ext"
			(prompt &optional default)
   :remove
   "Prompting with PROMPT, read a bookmark name in completion.
PROMPT will get a \": \" stuck on the end no matter what, so you
probably don't want to include one yourself.
Optional arg DEFAULT is a string to return if the user input is empty.
If DEFAULT is nil then return empty string for empty input."
   (bookmark-maybe-load-default-file) ; paranoia
   (if (listp last-nonmenu-event)
       (bookmark-menu-popup-paned-menu t prompt
				       (if bookmark-sort-flag
					   (sort (bookmark-all-names)
						 'string-lessp)
					 (bookmark-all-names)))
     (let* ((completion-ignore-case bookmark-completion-ignore-case)
            (default (unless (equal "" default) default))
	    (prompt (concat prompt (if default
                                       (format " (%s): " default)
                                     ": "))))
       (ido-completing-read prompt
			    (-sort #'string<
				   (cl-loop for (bookmark . _spec) in bookmark-alist
					    collect (cl-ext-progn
						      bookmark)))))))
 t)

;;;###autoload
(defun bookmark-bmenu--extra-hook () t)

;;;###autoload
(add-hook 'bookmark-bmenu-mode-hook #'bookmark-bmenu--extra-hook)

(define-key bookmark-bmenu-mode-map (kbd "<S-return>") #'bookmark-bmenu-ext-this-window-alternate)
(define-key bookmark-bmenu-mode-map (kbd "k") #'kill-and-quit)

(extension-provide 'bookmark-menu-ext)
;;; bookmark-menu-ext.el ends here
