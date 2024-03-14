(desktop+-default-bindings)

(defun contextual-key-s ()
  "Depending on the major mode, either switches to a different major mode or turns
on a minor mode."
  (interactive)
  (if (eq major-mode 'python-mode)
      (sphinx-doc-mode)
    (shell-script-mode)))

(defun contextual-key-y (mode)
  ;; choose from a list of yas-minor-mode or yas-global-minor-mode
  (interactive (list (completing-read "Choose: " '("yas-minor-mode" "yas-global-minor-mode"))))
  (call-interactively (intern mode)))

(defun lookup-function (fn &optional keymap firstonly)
  "Lookup the key binding for FN. If KEYMAP is non-nill, only search through that
keymap. Returns a list of keybinding descriptions unless FIRSTONLY is non-nill."
  (let ((all-bindings
	 (where-is-internal (if (symbolp fn)
				fn
			      (cl-first fn))
			    keymap))
	 keys key-bindings)
    (dolist (binding all-bindings)
      (when (and (vectorp binding)
		 (integerp (aref binding 0)))
	(push binding key-bindings)))
    (push (mapconcat #'key-description key-bindings " or ") keys)
    (if (null firstonly)
	keys
      (car keys))))

(defun map-revert-buffer ()
  (interactive)
  (local-set-key (kbd "M-R") #'revert-buffer)
  (message "Mapped %s `revert-buffer'"
	   (lookup-function 'revert-buffer nil t)))

(defun remap-narrow-to-region ()
  (interactive)
  (local-set-key [remap narrow-to-region] #'narrow-to-region2)
  (message "Remapped %s to `narrow-to-region2'"
	   (lookup-function 'narrow-to-region nil t)))

;;; Binds

;; Delete bookmark
(global-set-key (kbd "C-c d") 'bookmark-delete)

;; Tab bindings
(global-set-key (kbd "C-<tab>") 'tab-next)
(global-set-key (kbd "C-S-<iso-lefttab>") 'tab-previous)

;; Enable move-text bindings by default
(let ()
  (require 'move-text)
  (move-text-default-bindings))

;; prompt the user to choose from a list of comment-tags-mode or company-mode
(defun ctrl-c-m-c (choice)
  "Prompt the user to choose from a list of `comment-tags-mode' or `company-mode'."
  (interactive (list (completing-read "Choose: "
				      '("comment-tags-mode" "company-mode"))))
  (call-interactively (intern choice)))

;; Prefix command for quickly turning on different modes
(define-prefix-command 'quick-mode-map)
(global-set-key (kbd "C-c m") 'quick-mode-map)

(define-key quick-mode-map (kbd "a") #'abbrev-mode)
;; (define-key quick-mode-map (kbd "c") 'comment-tags-mode)
(define-key quick-mode-map (kbd "c") #'ctrl-c-m-c)
(define-key quick-mode-map (kbd "f") #'auto-fill-mode)
(define-key quick-mode-map (kbd "l") #'lsp-mode)
(define-key quick-mode-map (kbd "o") #'origami-mode)
(define-key quick-mode-map (kbd "p") #'p-mode)
(define-key quick-mode-map (kbd "r") #'auto-revert-mode)
(define-key quick-mode-map (kbd "s") #'contextual-key-s)
(define-key quick-mode-map (kbd "v") #'view-mode)
;; (define-key quick-mode-map (kbd "x") #'ex-python-mode)
(define-key quick-mode-map (kbd "w") #'enable-wrap)
(define-key quick-mode-map (kbd "y") #'contextual-key-y)

(global-set-key (kbd "C-x M-d") #'dired-alternate)

;; Prefix command for misc commands
(define-prefix-command 'command-map)
(global-set-key (kbd "C-c c") 'command-map)

(define-key command-map (kbd "c") #'comment-region)
(define-key command-map (kbd "d") #'copy-text-down)
(define-key command-map (kbd "s") #'nonincremental-search-forward)
(define-key command-map (kbd "M-s") #'nonincremental-search-backward)
(define-key command-map (kbd "%") #'nonincremental-re-search-forward)
(define-key command-map (kbd "M-%") #'nonincremental-re-search-backward)
(define-key command-map (kbd "p") #'forward-or-backward-sexp)

;; Misc commands which do not use my custom prefix
(global-set-key (kbd "C-M-k") 'copy-line)
(global-set-key (kbd "C-n") 'nonincremental-repeat-search-forward)
(global-set-key (kbd "C-M-n") 'nonincremental-repeat-search-backward)
(global-set-key (kbd "M-=") 'count-words-region2)
(global-set-key (kbd "C-c M-q") 'kill-and-quit)
