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

(defun dtext-ext-shift-return ()
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

(defun dtext-ext-insert-ext-link (url text &optional sitename sep)
  (interactive "sURL: \nsText: \nsSite Name: ")
  (save-excursion
    (if (not sep)
	(setq sep "-"))
    (if (and sitename (> (length sitename) 0))
	(insert (format "\"%s %s %s\":[%s]" text sep sitename url))
      (insert (format "\"%s\":[%s]" text url)))))

(defun dtext-ext-insert-ext-danbooru-link (url text)
  (interactive "sURL: \nsText: ")
  (dtext-ext-insert-ext-link url text "Danbooru" "on"))

(defun dtext--ext-hook ()
  (local-set-key (kbd "C-c i") 'dtext-mode-prefix)
  (define-key dtext-mode-prefix "l" #'dtext-ext-insert-ext-link)
  (define-key dtext-mode-prefix (kbd "M-l") #'dtext-ext-insert-ext-danbooru-link)
  ;; (define-key dtext-mode-map (kbd "C-c C-l M-l") #'dtext-ext-linkify)
  (define-key dtext-mode-map (kbd "<S-return>") #'dtext-ext-shift-return))

(add-hook 'dtext-mode-hook #'dtext--ext-hook)
