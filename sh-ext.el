;; Shell script mode extensions

(defun sh--extra-hook ()
  (setq imenu-generic-expression
	'(("*Functions*" "^function\\s-+\\([_a-z]+\\)" 1)
	  ("*Functions*" "^\\s-+\\([_a-z]+\\)()" 1)))
  (electric-pair-local-mode))

(add-hook 'sh-mode-hook #'sh--extra-hook)

(defconst sh-ext-function-regex
  "^\\(?:function\\s-+\\([A-Z_a-z]+\\)\\|\\(?1:[A-Z_a-z]+\\)()\\)"
  "Regular expression for finding functions.")

(defun sh-ext-occur-functions (&optional nlines)
  "Run `occur' on the current buffer for function definitions.
Each line is matched with `sh-ext-function-regex'.

Optional arg NLINES is interpreted the same way as for
`occur'. Interactively it is the prefix arg.

Optional args BEG and END, are used to restrict search to a
designated region. Interactively, the region is used to set
these args."
  (interactive "p")
  (let (beg end)
    (when (use-region-p)
      (setq beg (region-beginning) end (region-end)))
    (occur sh-ext-function-regex
	   (if (= nlines 1) -2 nlines)
	   (if (use-region-p)
	       (list beg end)))
    (with-current-buffer "*Occur*"
      ;; Buffer defined = kill buffer
      (let* ((bufname "*Occur: sh functions*")
	     (buf (get-buffer bufname)))
	(when buf
	  (kill-buffer bufname))
	(highlight-parentheses-mode -1)
	(rename-buffer bufname)
	(local-set-key "q" #'kill-and-quit)))))
