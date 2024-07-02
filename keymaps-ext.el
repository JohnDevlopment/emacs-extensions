(require 'move-text)
(require 'desktop+)

(move-text-default-bindings)
(desktop+-default-bindings)

(defmacro define-contextual-key-func (key &rest modes)
  "Define a function that enables one of the modes.
KEY is a string name for the key to press.

MODE is a string that is the name of a mode. If there is
only one mode, this is expanded into a function call.

\(fn KEY MODE ...)"
  (declare (indent 1))
  (let (doc)
    (if (= (length modes) 1)
	`(progn
	   ,(setq doc (format "Call `%s'.\nLikely called from C-c C-m %s."
			      (car modes) key))
	   (defun ,(intern (concat "contextual-key-" key)) ()
	     ,doc
	     (interactive)
	     ;; MODES is one mode, so call it directly
	     (call-interactively #',(intern (car modes)))))
      `(progn
	 ,(setq doc (format "Choose from a list of modes to enable.\nLikely called from C-c C-m %s."
			    key))
	 (defun ,(intern (concat "contextual-key-" key))
	     (choice)
	   ,doc
	   (interactive (list (completing-read "Choose: " ',modes)))
	   (call-interactively (intern choice)))))))

(eval-and-compile
  (define-prefix-command 'command-map)
  (define-prefix-command 'quick-mode-map))

(define-contextual-key-func "a"
  "abbrev-mode"
  "auto-fill-mode"
  "auto-revert-mode")

(define-contextual-key-func "c" "comment-tags-mode" "company-mode")

(define-contextual-key-func "g"
  "global-company-mode"
  "global-electric-pair-mode"
  "global-visual-line-mode"
  "yas-global-mode")

(defun contextual-key-s ()
  "Depending on the major mode, either switches to a different major mode or turns
on a minor mode."
  (interactive)
  (if (eq major-mode 'python-mode)
      (sphinx-doc-mode)
    (shell-script-mode)))

(define-contextual-key-func "y" "yas-minor-mode")

(defun lookup-function (fn &optional keymap firstonly)
  "Lookup the key binding for FN. If KEYMAP is non-nil,
only search through that keymap.

Returns a list of keybinding descriptions unless FIRSTONLY
is non-nill."
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

;; Bind `view-file' to C-x M-v
(global-set-key (kbd "C-x M-v") #'view-file)

;; Delete bookmark
(global-set-key (kbd "C-c d") #'bookmark-delete)

;; Tab bindings
(global-set-key (kbd "C-<tab>")           #'tab-next)
(global-set-key (kbd "C-S-<iso-lefttab>") #'tab-previous)

;; Prefix command for quickly turning on different modes
(global-set-key (kbd "C-c m") #'quick-mode-map)

;; (define-key quick-mode-map (kbd "a") #'abbrev-mode)
(define-key quick-mode-map (kbd "a") #'contextual-key-a)
(define-key quick-mode-map (kbd "c") #'contextual-key-c)
;; (define-key quick-mode-map (kbd "f") #'auto-fill-mode)
(define-key quick-mode-map (kbd "g") #'contextual-key-g)
(define-key quick-mode-map (kbd "l") #'lsp-mode)
(define-key quick-mode-map (kbd "o") #'origami-mode)
(define-key quick-mode-map (kbd "p") #'p-mode)
;; (define-key quick-mode-map (kbd "r") #'auto-revert-mode)
(define-key quick-mode-map (kbd "s") #'contextual-key-s)
(define-key quick-mode-map (kbd "v") #'view-mode)
(define-key quick-mode-map (kbd "w") #'enable-wrap)
(define-key quick-mode-map (kbd "y") #'contextual-key-y)

;; Prefix command for misc commands
(global-set-key (kbd "C-c c") #'command-map)

(define-key command-map (kbd "%")   #'nonincremental-re-search-forward)
(define-key command-map (kbd "M-%") #'nonincremental-re-search-backward)
(define-key command-map (kbd "M-s") #'nonincremental-search-backward)
(define-key command-map (kbd "c")   #'comment-region)
(define-key command-map (kbd "d")   #'copy-text-down)
(define-key command-map (kbd "p")   #'forward-or-backward-sexp)
(define-key command-map (kbd "s")   #'nonincremental-search-forward)
(define-key command-map (kbd "'")   #'save-current-position)
(define-key command-map (kbd "&")   #'pop-saved-position)

;; Misc commands which do not use my custom prefix
(global-set-key (kbd "C-M-k")   #'copy-line)
(global-set-key (kbd "C-M-n")   #'nonincremental-repeat-search-backward)
(global-set-key (kbd "C-c M-q") #'kill-and-quit)
(global-set-key (kbd "C-n")     #'nonincremental-repeat-search-forward)
(global-set-key (kbd "M-=")     #'count-words-region2)
;; (global-set-key (kbd "C-x C-b") #'list-buffers-ex)
(global-set-key (kbd "C-x C-b") #'ibuffer)
