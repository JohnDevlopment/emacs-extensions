# Emacs Extensions

```elisp
(defun load-extension (extension &optional noerror nomessage)
  "Loads an extension. Looks for a file under
'~/.emacs.d/extensions' named EXTENSION.

EXTENSION does not require an extension.

Optional args NOERROR and NOMESSAGE are forwarded to `load'."
  (interactive
   (let* (completion-ignored-extensions
	 (path "~/.emacs.d/extensions"))
     (list (completing-read "Load extension: "
			    (apply-partially 'locate-file-completion-table
					     (list path)
					     (get-load-suffixes))))))
  (load (concat "~/.emacs.d/extensions/" extension) noerror nomessage))

(defun find-extension (extension)
  "Find the Emacs Lisp source of EXTENSION."
  (interactive
   (let* (completion-ignored-extensions
	 (path "~/.emacs.d/extensions"))
     (list (completing-read "Load extension: "
			    (apply-partially 'locate-file-completion-table
					     (list path)
					     (get-load-suffixes))))))
  (prog1
      (switch-to-buffer (find-file-noselect
			 (concat "~/.emacs.d/extensions/" extension ".el")))))

;; Extensions
(load-extension "extensions")
```
