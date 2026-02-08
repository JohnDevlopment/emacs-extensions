;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.4")

(require 'info)

;;;###autoload
(defun info-node (node &optional buffer)
  "Enter Info, the documentation browser, for NODE.
NODE is an Info node of the form \"(FILENAME)NODENAME\".  Optional
argument BUFFER specifies the Info buffer name; the default buffer
name is \"*info*\".  If BUFFER exists, just switch to it; otherwise,
create a new buffer with the top-level Info directory.

In interactive use, a numeric prefix argument of N selects an Info
buffer named \"*info*<N>\".

Aside from these differences, this works exactly the same as
`info'."
  (interactive (list
		(read-string "Info node: ")
		(and (numberp current-prefix-arg)
		     (format "*info<%s>" current-prefix-arg))))
  (info-setup node
	      (pop-to-buffer-same-window (or buffer "*info*"))))

;;;###autoload
(defun info-node-other-window (node &optional buffer)
  "Like `info-node' but show buffer in another window."
  (interactive (list
		(read-string "Info node: ")
		(and (numberp current-prefix-arg)
		     (format "*info<%s>" current-prefix-arg))))
  (info-setup node
	      (pop-to-buffer (or buffer "*info*"))))

(extension-provide 'info)
;;; info.el ends here
