;;; dtext-ext --- DText mode extension.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'dtext-mode nil t))

(eval-and-compile
  (define-prefix-command 'dtext-mode-prefix))

;; (defun dtext-ext-linkify (start end)
;;   "Surround the text in region START and END with double brackets."
;;   (interactive "r")
;;   (when (use-region-p)
;;     (let ((substr (buffer-substring start end)))
;;       (delete-region start end)
;;       (insert "[[" substr "]]")
;;       (deactivate-mark)
;;       (goto-char (+ start 2)))))

(defconst dtext-ext-list-regexp
  "^\\(\\*+\\)"
  "Regular expression for list bullets with leading spaces.

Group 1 matches one or more bullets (*).")

;;;###autoload
(defun dtext-ext-shift-return ()
  "Handle S-<return>.
If point is inside a list, then start a new item."
  (interactive)
  (let (bl-beg bl-end str)
    (save-excursion
      (save-match-data
	(beginning-of-line)
	(when (looking-at dtext-ext-list-regexp)
	  (setq bl-beg (match-beginning 1) bl-end (match-end 1)
		str (buffer-substring bl-beg bl-end)))))
    (newline 1 t)
    (insert str " ")))

;;;###autoload
(defun dtext-ext-insert-ext-link (url text &optional sitename sep)
  "Insert a link to URL at point with TEXT.

If the arguments SITENAME and SEP are provided, the link
will say something like \"TEXT SEP SITENAME\".  Without, the
link will just have text.  If SEP is not provided, it
defaults to \"-\".

Interactively, prompts the user for URL, TEXT, and SITENAME."
  (interactive "sURL: \nsText: \nsSite Name: ")
  (save-excursion
    (unless sep
	(setq sep "-"))
    (if (and sitename (> (length sitename) 0))
	(insert (format "\"%s %s %s\":[%s]" text sep sitename url))
      (insert (format "\"%s\":[%s]" text url)))))

;;;###autoload
(defun dtext-ext-insert-ext-danbooru-link (url text)
  "Insert a link pointing to URL with TEXT.
This simply calls `dtext-ext-insert-ext-link' with
\"Danbooru\" as SITENAME and \"on\" as SEP."
  (interactive "sURL: \nsText: ")
  (dtext-ext-insert-ext-link url text "Danbooru" "on"))

;;;###autoload
(defun dtext--ext-hook ()
  "Extra hook for `dtext-mode'."
  (local-set-key (kbd "C-c i") 'dtext-mode-prefix)
  (define-key dtext-mode-prefix "l" #'dtext-ext-insert-ext-link)
  (define-key dtext-mode-prefix (kbd "M-l") #'dtext-ext-insert-ext-danbooru-link)
  ;; (define-key dtext-mode-map (kbd "C-c C-l M-l") #'dtext-ext-linkify)
  (define-key dtext-mode-map (kbd "<S-return>") #'dtext-ext-shift-return))

(add-hook 'dtext-mode-hook #'dtext--ext-hook)

(provide 'dtext-ext)

;;; dtext-ext ends here
