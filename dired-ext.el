;; -*- lexical-binding: t; -*-

(require 'dired)
(require 'dired-x)
(require 'f)

(eval-when-compile
  (require 'function-ext)
  (require 'debug-ext))

;; Assertions
(check-emacs-minimum-version "27.1")
(extension-check-requires function-ext)


;; ### Functions

(defsubst dired-ext--barf-if-not-dired-mode ()
  (unless (derived-mode-p 'dired-mode)
    (user-error "Must be in a Dired buffer")))

(defun dired-ext-quit-kill-window ()
  "Kill the current buffer and quit the window."
  (interactive nil dired-mode)
  (quit-window t))

(defun dired-alternate (dirname &optional switches)
  "\"Edit\" directory DIRNAME, same as `dired', kill the current buffer.

DIRNAME is either a string or a cons: as a string, the name
of the directory to edit; as a cons, the first element is
the directory, and the rest are a list of files to make
directory entries for.

In this case, SWITCHES are applied to each file
individually, so list-sorting options are pointless."
  (emacs-version-cond-when-compile
    ((>= "29")
     (interactive (dired-read-dir-and-switches "") dired-mode))
    (t (interactive (dired-read-dir-and-switches ""))))
  (let ((curbuf (current-buffer))
	(newbuf (dired-noselect dirname switches)))
    (pop-to-buffer-same-window newbuf)
    (kill-buffer curbuf)))

(defun dired-ext-find-alternate-updir ()
  "In Dired, find alternate file in parent directory."
  (emacs-version-cond-when-compile
    ((>= "29")
     (interactive nil dired-mode))
    (t (interactive nil)))
  (dired-ext--barf-if-not-dired-mode)
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
  (emacs-version-cond-when-compile
    ((>= "29")
     (interactive (dired-read-dir-and-switches "") dired-mode))
    (t (interactive (dired-read-dir-and-switches ""))))
  (dired-ext--barf-if-not-dired-mode)
  (let ((newbuf (dired-noselect
		 (mapcar (lambda (str) (f-join dirname str)) '("*/" ".*/")) "-adl")))
    (pop-to-buffer-same-window newbuf)))


;; ### Dired Omit Mode

(defun dired-ext-require-omit-mode (&rest _r)
  "Display an error if `dired-omit-mode' is not enabled."
  (unless (bound-and-true-p dired-omit-mode)
    (user-error "Enable `dired-omit-mode'")))

(--ignore :no-warn
  (advice-add #'dired-omit-expunge :before #'dired-ext-require-omit-mode)
  (advice-remove #'dired-omit-expunge #'dired-ext-require-omit-mode)
  t)
(define-key dired-mode-map (kbd "M-k") #'dired-omit-expunge)


;; ### Keybinds

(put #'dired-find-alternate-file 'disabled nil)

(global-set-key (kbd "C-x M-d") #'dired-alternate)
(global-set-key (kbd "C-x C-M-d") #'dired-dirs)

(define-key dired-mode-map "^" #'dired-ext-find-alternate-updir)
(define-key dired-mode-map "k" #'dired-ext-quit-kill-window)


;; ### Hook

;;;###autoload
(defun dired-mode--extra-hook ()
  "Extra hook for `dired-mode'.")

;;;###autoload
(add-hook 'dired-mode-hook #'dired-mode--extra-hook)


(extension-provide 'dired-ext)
;;; dired-ext ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "dx" "dired-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-dired")
;; End:
