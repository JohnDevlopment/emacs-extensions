;; -*- lexical-binding: t; -*-

(require 'dired)

;; Enable `dired-find-alternate-file'
(put #'dired-find-alternate-file 'disabled nil)

;; Bind C-x M-d to `dired-alternate'
(global-set-key (kbd "C-x M-d") #'dired-alternate)

;; Variables

(document-extension "dired-ext"
  "Dired mode extension.

Key binds:
^       `dired-ext-find-alternate-updir'
k       `dired-ext-quit-kill-window'"
  :functions ((dired-ext-find-alternate-updir command)
	      (dired-ext-quit-kill-window command)
	      (dired-alternate command)))

;; Functions

(defun dired-ext-find-alternate-updir ()
  "In Dired, find alternate file in parent directory."
  (interactive)
  (find-alternate-file ".."))

(defun dired-ext-quit-kill-window ()
  "Kill the current buffer and quit the window."
  (interactive)
  (quit-window t))

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

;; Hook

(define-key dired-mode-map "^" #'dired-ext-find-alternate-updir)
(define-key dired-mode-map "k" #'dired-ext-quit-kill-window)

(defun dired-mode--extra-hook ()
  "Extra hook for `dired-mode'.")

(add-hook 'dired-mode-hook #'dired-mode--extra-hook)

(provide 'dired-ext)

;;; dired-ext ends here
