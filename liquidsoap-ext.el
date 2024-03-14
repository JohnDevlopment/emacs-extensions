;;; liquidsoap-ext.el --- Liquidsoap mode extension

(eval-when-compile
  (require 'liquidsoap-mode)
  (require 'liquidsoap-completions))

(eval-and-compile
  (require 'debug-ext))

(defgroup user-ext-liquidsoap-mode nil
  "A group for `liquidsoap-mode'."
  :group 'user-extensions)

(defface liquidsoap-help-arg
  '((t (:inherit font-lock-variable-name-face)))
  "A face for arguments in a `liquidsoap-mode' help buffer."
  :group 'user-ext-liquidsoap-mode)

(defface liquidsoap-help-type
  '((t (:inherit font-lock-type-face)))
  "A face for argument types in a `liquidsoap-mode' help buffer."
  :group 'user-ext-liquidsoap-mode)

(defun liquidsoap-ext--get-doc (command)
  "Get documentation for COMMAND."
  (list (get-text-property 0 :description command)
	(get-text-property 0 :type command)))

(defun liquidsoap-ext--complete-command ()
  "Get a command from `liquidsoap-completions' via completion."
  (let* ((command (completing-read "Command: " liquidsoap-completions))
	 (result (cl-find command liquidsoap-completions :test 'string=)))
    (assert (not (null result)))
    (list result)))

(defun liquidsoap-ext-get-doc (command)
  (interactive (liquidsoap-ext--complete-command))
  (with-help-window (help-buffer)
    (cl-destructuring-bind (doc type) (liquidsoap-ext--get-doc command)
      (princ (format "%s%s\n\n%s" command type doc)))
    (help-setup-xref (list 'liquidsoap-ext-get-doc command)
		     (called-interactively-p 'interactive))
    (with-current-buffer (help-buffer)
      (let ((arg-regexp "\\??\\([a-z_]+?\\) :")
	    (arg-regexp-type ": \\([a-z_]+\\)\\??,?")
	    (arg-regexp-return-type " -> \\([a-z_]+\\)"))
	(highlight-regexp arg-regexp 'liquidsoap-help-arg 1)
	(highlight-regexp arg-regexp-type 'liquidsoap-help-type 1)
	(highlight-regexp arg-regexp-return-type 'liquidsoap-help-type 1)))))
