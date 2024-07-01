(eval-and-compile
  (require 'dired))

(put 'dired-find-alternate-file 'disabled nil)



(defun dired-alternate (dirname &optional switches)
  "\"Edit\" directory DIRNAME, same as `dired', kill the current buffer.

DIRNAME is either a string or a cons: as a string, the name
of the directory to edit; as a cons, the first element is
the directory, and the rest are a list of files to make
directory entries for.

In this case, SWITCHES are applied to each file
individually, so list-sorting options are pointless."
  (interactive (dired-read-dir-and-switches ""))
  (let ((curbuf (current-buffer))
	(newbuf (dired-noselect dirname switches)))
    (pop-to-buffer-same-window newbuf)
    (kill-buffer curbuf)))

;;; Hook

(defun dired-mode--extra-hook ()
  "Extra hook for `dired-mode'."
  (define-key dired-mode-map "^" #'dired-ext-find-alternate-updir)
  (define-key dired-mode-map "k" #'dired-ext-quit-kill-window))

(add-hook 'dired-mode-hook #'dired-mode--extra-hook)
