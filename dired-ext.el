;; -*- lexical-binding: t; -*-

(require 'dired)

(eval-when-compile
  (require 'function-ext))

;; Functions

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

(defun dired-ext-find-alternate-updir ()
  "In Dired, find alternate file in parent directory."
  (interactive)
  (when (derived-mode-p 'dired-mode)
    ;; (dired-listing-switches dired-actual-switches)
    ;; (find-alternate-file "..")
    ;; dired-actual-switches
    (dired-alternate "..")))

(defun dired-dirs (dirname &optional _switches)
  "\"Edit\" directory DIRNAME like `dired', but list only directories.

DIRNAME is either a string or a cons: as a string, the name
of the directory to edit; as a cons, the first element is
the directory, and the tail is a list of files to make
directory entries for."
  (interactive (dired-read-dir-and-switches ""))
  (let ((newbuf (dired-noselect
		 (mapcar (lambda (str) (f-join dirname str)) '("*/" ".*/")) "-adl")))
    (pop-to-buffer-same-window newbuf)))

;; Keybinds

;; Enable `dired-find-alternate-file'
(put #'dired-find-alternate-file 'disabled nil)

;; Bind C-x M-d to `dired-alternate'
(global-set-key (kbd "C-x M-d") #'dired-alternate)

;; Bind C-x C-M-d to `dired-dirs'
(global-set-key (kbd "C-x C-M-d") #'dired-dirs)

;; Hook

(define-key dired-mode-map "^" #'dired-ext-find-alternate-updir)
(define-key dired-mode-map "k" #'dired-ext-quit-kill-window)

(defun dired-mode--extra-hook ()
  "Extra hook for `dired-mode'.")

(add-hook 'dired-mode-hook #'dired-mode--extra-hook)

(provide 'dired-ext)

;;; dired-ext ends here
