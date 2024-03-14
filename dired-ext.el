(eval-and-compile
  (require 'dired))

(put 'dired-find-alternate-file 'disabled nil)

(defun dired-mode--extra-hook ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map "k"
    (lambda () (interactive) (quit-window t))))

(add-hook 'dired-mode-hook #'dired-mode--extra-hook)

(defun dired-alternate (dirname &optional switches)
  "Calls `dired' with the provided arguments, with the additional function of
killing the current buffer. In that sense, it acts like `find-alternate-file'.

Quick summary of options: DIRNAME is either a string or a cons, as a string, the
name of the directory to edit; and as a cons, the first element is the
directory, and the rest are a list of files to make directory entries for. In
this case, SWITCHES are applied to each file individually, so list-sorting
options are pointless."
  (interactive (dired-read-dir-and-switches ""))
  (let ((curbuf (current-buffer))
	(newbuf (dired-noselect dirname switches)))
    (pop-to-buffer-same-window newbuf)
    (kill-buffer curbuf)))
