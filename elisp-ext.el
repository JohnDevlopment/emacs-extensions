;;; elisp-ext --- Emacs Lisp mode extension.  -*- lexical-binding: t;  -*-

;;; Commentary:

;;; Code:

;; (require 'debug-ext)

;; Variables

(defconst user-ext-elisp--register ?e
  "Used with `window-configuration-to-register'.")

;; Functions

;;;###autoload
(defun elisp-ext-minify (start end)
  "Minify the code between START and END in current buffer.
If called interactively, START and END are the region,
provided the region is active.  But if the region is not
active, the entire buffer is minified."
  (interactive "r")
  (cl-block quit
    (let* ((msg "The region is not active, so the entire buffer will be minified. Continue?")
	   (reg (region-active-p))
	   answer
	   bstr)
      (setq answer (and reg (y-or-n-p msg)))
      (cond
       ((and (not reg) (not answer))
	;; Quit; user said no and the region is not active
	(message "Quit")
	(cl-return-from quit))
       ((and (not reg) answer)
	;; User said yes but the region is not active
	(setq start (point-min) end (point-max))))
      (setq bstr (buffer-substring-no-properties start end))
      (delete-region start end)
      (setq bstr (replace-regexp-in-string "[ \t\n\r]+" " " bstr))
      (insert bstr))))

;;;###autoload
(defun elisp-ext-forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis to the one is adjacent at point.
With ARG, do it that many times.  A negative arg -N reverses
the direction of the motion."
  (interactive "^p")
  (cond ((looking-at "\\s(")
	 (forward-sexp arg))
	((looking-at "\\s)")
	 (forward-char)
	 (backward-sexp arg))
        ((looking-back "\\s)" 1)
	 (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-back "\\s(" 1)
	 (backward-char)
	 (forward-sexp arg)
	 (backward-char))))

;;;###autoload
(defun elisp-ext-update-loadefs (&optional interactive-p)
  "Update autoload definitions of Lisp extensions.

Updates the autoload definitions in the Lisp files in
`user-ext-extension-directory'.  Binds
`generated-autoload-file' to the concatenation of
`user-ext-extension-directory' and \"loaddefs-ext.el\"."
  (interactive "p")
  (unless interactive-p
    (error "This must be called interactively"))
  (let ((generated-autoload-file "~/.emacs.d/extensions/loaddefs-ext.el"))
    (update-directory-autoloads "~/.emacs.d/extensions/"))
  (when (y-or-n-p "Open buffer \"loaddefs-ext.el\"? ")
    (switch-to-buffer "loaddefs-ext.el")))
(put 'elisp-ext-update-loadefs 'interactive-only t)

(defun elisp-ext--scratch-buffer-ctrl-c-ctrl-c ()
  "Kill the text inside buffer and quit."
  (interactive)
  (kill-region (point-min) (point-max))
  (kill-and-quit))

;;;###autoload (autoload 'elisp-ext-scratch-buffer "elisp-ext" "Create a scratch buffer for Emacs lisp code." t)
(define-scratch-buffer-function elisp-ext-scratch-buffer "elisp" nil
  "Create a scratch buffer for Emacs lisp code."
  nil
  (emacs-lisp-mode)
  (unless (and (boundp 'electric-pair-mode) electric-pair-mode) ; Enable electric pair mode
    (electric-pair-local-mode t))				;
  (unless (and (boundp 'company-mode) company-mode) ; Enable company mode
    (company-mode t))				    ;
  (setq header-line-format "Type C-c C-c when finished, C-x k to cancel editing.")
  (local-set-key (kbd "C-c C-c") #'elisp-ext--scratch-buffer-ctrl-c-ctrl-c))

(defun elisp-ext--find-quote (&optional reverse)
  "Find the next quote.
If REVERSE is non-nil, search backwards."
  (save-excursion
    (if reverse
	(progn
	  (goto-char (point-max))
	  (search-backward "\"")
	  (point))
      (goto-char (point-min))
      (search-forward "\"")
      (point))))

(defun elisp-ext-hide-all ()
  "Hide all toplevel forms in buffer.
Automatically activates `hs-minor-mode' when called."
  (interactive)
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (hs-hide-all))

(defun elisp-ext-show ()
  "Show the form at or before point."
  (interactive)
  (when (= (char-before) ?\))
    (left-char))
  (when (not hs-minor-mode)
    (hs-minor-mode 1))
  (hs-show-block))

(defun elisp-ext-hide-toplevel-form ()
  "Hide the function containing point.
Automatically activates `hs-minor-mode' when called."
  (interactive)
  (when (= (char-before) ?\))
    (left-char))
  (let* ((ppss (make-ppss-easy (syntax-ppss)))
	 (tl (syntax-ppss-toplevel-pos ppss))
	 p)
    (when tl
      (cl-save-point
	(goto-char tl)
	(setq p tl)))
    (when p
      (goto-char p)
      (when (not hs-minor-mode)
	(hs-minor-mode 1))
      (hs-hide-block))))

;; Elisp doc minor mode

(defun elisp-ext-doc-scratch-buffer--shift-return ()
  "Insert a newline and change fill column."
  (interactive)
  (newline)
  (set-fill-column 60))

(defun elisp-ext-doc-scratch-buffer--ctrl-c-ctrl-c ()
  "Kill the text inside this buffer and restore window."
  (interactive)
  (let* ((beg (1+ (elisp-ext--find-quote)))
	 (end (1- (elisp-ext--find-quote t))))
    (kill-region beg end)
    (kill-buffer)
    (jump-to-register user-ext-elisp--register)))

(define-scratch-buffer-function elisp-ext--doc-scratch-buffer "elisp docstring" nil
  "Create a scratch buffer for Emacs lisp docstrings."
  nil
  (emacs-lisp-mode)
  (elisp-ext-doc-minor-mode 1)
  (auto-fill-mode t)
  (set-fill-column 67)
  (unless (and (boundp 'electric-pair-mode) electric-pair-mode)
    (electric-pair-local-mode t))
  (unless (and (boundp 'company-mode) company-mode)
    (company-mode t))
  (insert ";; Fill column is set to 67. Type S-<return> to set it to 60.\n"
	  ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
	  "\"\n\n\"")
  (goto-char (1+ (elisp-ext--find-quote)))
  (setq header-line-format "Type C-c C-c when finished."))

;;;###autoload
(defun elisp-ext-doc-scratch-buffer ()
  "Create a scratch buffer for Emacs Lisp docstrings."
  (interactive)
  (window-configuration-to-register user-ext-elisp--register)
  (elisp-ext--doc-scratch-buffer))

(define-minor-mode elisp-ext-doc-minor-mode
  "Minor mode for `elisp-ext-doc-scratch-buffer'.

\\{elisp-ext-doc-minor-mode-map}"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<S-return>") #'elisp-ext-doc-scratch-buffer--shift-return)
	    (define-key map (kbd "C-c C-c") #'elisp-ext-doc-scratch-buffer--ctrl-c-ctrl-c)
	    map))

;; Keymap

(define-prefix-command 'user-ext-elisp-fold-map)
(define-key emacs-lisp-mode-map (kbd "C-c f") #'user-ext-elisp-fold-map)
(define-key user-ext-elisp-fold-map (kbd "t") #'elisp-ext-hide-toplevel-form)
(define-key user-ext-elisp-fold-map (kbd "s") #'elisp-ext-show)
(define-key user-ext-elisp-fold-map (kbd "C-a") #'elisp-ext-hide-all)

(define-key emacs-lisp-mode-map (kbd "C-c C-j") #'imenu)
(define-key emacs-lisp-mode-map (kbd "C-c c b") #'emacs-lisp-byte-compile)
(define-key emacs-lisp-mode-map (kbd "C-c c M-b") #'emacs-lisp-byte-compile-and-load)

;; Hook

;;;###autoload
(defun elisp-ext--extra-hook ()
  "Hook for the `emacs-lisp-mode' extension.")

;;;###autoload
(add-hook 'emacs-lisp-mode-hook #'elisp-ext--extra-hook)

(provide 'elisp-ext)

;;; elisp-ext ends here
