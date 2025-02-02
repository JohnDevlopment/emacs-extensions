;;; rust-ext --- Rust mode extension.  -*- lexical-binding: t; -*-

(require 'company-capf)
(require 'rustic)
(require 'rust-mode)

(eval-when-compile
  (require 'skeleton))

;; Variables

(defgroup rust-ext nil
  "A group for Rust extension."
  :group 'user-extensions)

(defcustom user-ext-rust-docstring-fill-column 60
  "Fill column for docstring."
  :type 'integer
  :group 'rust-ext)

(defconst user-ext-rust--register ?r)

(defvar user-ext-rust--docstring-buffer nil
  "Docstring buffer in `rust-mode'.")

(defconst user-ext-rust-function-regexp
  (rx (* whitespace)
      (group "fn" (+ whitespace)
	     (+ (or (syntax word) (syntax symbol)))) ; function name
      (? ?< (+ nonl) ?>)			     ; type parameter list (i.e., <S: ToString>)
      ?\( (* nonl) ?\)				     ; parameter list
      (* whitespace) "->" (* whitespace)
      (+ (or (syntax word) (syntax symbol))) ; return type (i.e., "-> Self")
      (* (any "\n" whitespace)))
  "Matches the signature of a function.")

(defconst user-ext-rust-impl-regexp
  (rx bol (* whitespace)
      (group "impl" (+ whitespace) (+ (syntax word)))))

;; Functions

;; ---Syntax functions

(defun rust-ext-forward-statement ()
  (unless (eobp)
    (let ((ppss (make-ppss-easy (syntax-ppss)))
	  pos)
      (cond
       ))))

(defun rust-ext--re-search-backward (regexp &optional group)
  "Search backward for REGEXP and return the position.
This matches the entire regular expression unless GROUP is
non-nil, in which case that group is matched."
  (cl-check-type regexp string)
  (cl-check-type group integer-or-null)
  (let* ((group (or group 0))
	 (match (re-search-backward regexp nil t))
	 (pos (when match (match-beginning group))))
    (when pos (goto-char pos))
    pos))

(defun rust-ext--jump-backward (what)
  "Jump backward to the form WHAT.
WHAT is a symbol designating what form to choose."
  (cl-check-type what symbol)
  (unless (bobp)
    (let* ((args (rust-ext--jump-forward-backward-args what))
	   (pos (apply #'rust-ext--re-search-backward args)))
      (when pos
	(goto-char pos)))))

(defun rust-ext--re-search-forward (regexp &optional group)
  "Search forward for REGEXP and return the position.
This matches the entire regular expression unless GROUP is
non-nil, in which case that group is matched."
  (cl-check-type regexp string)
  (cl-check-type group integer-or-null)
  (let* ((group (or group 0))
	 (count (if (looking-at-p regexp) 2 nil))
	 (match (re-search-forward regexp nil t count))
	 (pos (and match (match-beginning group))))
    (print-expr var count)
    (when pos (goto-char pos))
    pos))

(defun rust-ext--jump-forward (what)
  "Jump forward to the form WHAT."
  (cl-check-type what symbol)
  (unless (bobp)
    (let* ((args (rust-ext--jump-forward-backward-args what))
	   (pos (apply #'rust-ext--re-search-forward args)))
      (when pos (goto-char pos)))))

(defun rust-ext--jump-args (what)
  (cl-ecase what
    (fn (list user-ext-rust-function-regexp 1))
    (impl (list user-ext-rust-impl-regexp 1))))
(defalias 'rust-ext--jump-forward-backward-args #'rust-ext--jump-args)
(make-obsolete 'rust-ext--jump-forward-backward-args #'rust-ext--jump-args "2025.02.02")

(cl-defmacro rust-ext-define-jump-function (name direction &key doc-name)
  "Create a motion function called rust-ext-jump-DIRECTION-NAME.
DIRECTION is either symbol `backward' or symbol `forward'.
NAME is a symbol.

This creates a function called rust-ext-jump-DIRECTION-
NAME.  The documentation string will use NAME and
DIRECTION when describing what the function does.  The lone
keyword argument :DOC-NAME replaces NAME in the
documentation string."
  (cl-check-type name symbol)
  (cl-check-type direction symbol)
  (cl-assert (memq direction '(forward backward))
	     nil "Must be %S" name '(forward backward))
  (cl-check-type doc-name string-or-null)
  (when (> (length doc-name) 15)
    (error "Invalid :doc-name '%s', length must be <= 15 characters" doc-name))
  (let* ((fname (intern (format "rust-ext-%s-%s" direction name)))
	 (doc-name (or doc-name (symbol-name name)))
	 (prev-or-next (if (eq direction 'backward)
			   "previous"
			 "next"))
	 (docstring (s-lex-format "Jump ${direction} to the beginning of a ${doc-name}.
If point is inside a ${doc-name}, move to the beginning,
else move to the beginning of a ${prev-or-next} ${doc-name}.")))
    `(progn
       (defun ,fname ()
	 ,docstring
	 (interactive)
	 (,(intern-soft (format "rust-ext--jump-%S" direction)) ',name)))))

(rust-ext-define-jump-function fn backward :doc-name "function")
(rust-ext-define-jump-function impl backward :doc-name "implementation")

(rust-ext-define-jump-function fn forward :doc-name "function")
(rust-ext-define-jump-function impl forward :doc-name "implementation")

;; ---Hide-show functions

(defun rust-ext-hide-base (form)
  (cl-check-type form symbol)
  (hs-minor-mode 1)
  (let* ((form (prin1-to-string form))
	 (func (intern-soft (s-lex-format "rust-ext-backward-${form}")))
	 (beg (save-excursion (funcall func)))))
  beg)

;; ---

(defun rust-ext-finish-variable-type ()
  "Complete the type of a variable using `company-capf'.
Point must be at the end of a variable binding where the
type usually goes."
  (interactive)
  (insert ": ")
  (call-interactively #'company-capf))

;;;###autoload
(defun rust-ext-cargo-run-with-args ()
  "Run this project via 'cargo run'.
This calls `rustic-cargo-run' with a non-nil argument."
  (interactive)
  (rustic-cargo-run 4))

(defun rust-ext--write-docstring ()
  "Exit the Rust docstring buffer and apply the docstring."
  (interactive)
  (let (buffer-string docstring)
    (deactivate-mark)
    (untabify (point-min) (point-max))
    (with-current-buffer user-ext-rust--docstring-buffer
      (setq buffer-string (buffer-string)))
    ;; exit if buffer is empty
    (when (string-blank-p buffer-string)
      (kill-buffer)
      (jump-to-register user-ext-rust--register)
      (error "Docstring is empty"))
    (kill-buffer)
    (jump-to-register user-ext-rust--register)
    (save-excursion
      (setq docstring
	    (string-join
	     (cl-loop
	      for line in (split-string buffer-string "\n")
	      collect (format "/// %s" line)) "\n"))
      (insert docstring))))

;;;###autoload
(defun rust-ext-docstring ()
  "Open a temporary buffer to write a docstring."
  (interactive)
  (let (modes)
    (window-configuration-to-register user-ext-rust--register)
    (dolist (mode '(abbrev-mode))	; we want to enable minor modes
      (when (and (boundp mode) mode)	; that were enabled in the
	(cl-pushnew mode modes)))	; original buffer
    (setq user-ext-rust--docstring-buffer (tmpbuf "rust-docstring"))
    (cl-assert (not (null user-ext-rust--docstring-buffer)))
    (split-window-sensibly)				 ;
    (switch-to-buffer user-ext-rust--docstring-buffer)   ; Split window and enter temporary buffer
    (markdown-mode)
    (dolist (mode modes)		; enable minor
      (funcall mode 1))			; modes from earlier
    (set-fill-column user-ext-rust-docstring-fill-column)
    (auto-fill-mode 1)
    (local-set-key (kbd "C-c C-c") #'rust-ext--write-docstring)
    (setq header-line-format "Rust Docstring: Type C-c C-c to apply changes")
    (message "Type C-c C-c to save changes.")))

;;;###autoload
(defun rust-ext-customize-group ()
  "Customize the group for `rusti-mode'."
  (interactive)
  (customize-group 'rustic))

;;;###autoload (autoload 'rust-ext-scratch-buffer "rust-ext" "Open a scratch buffer in `rustic-mode'." t)
(define-scratch-buffer-function rust-ext-scratch-buffer "rust" nil
  "Open a scratch buffer in `rustic-mode'."
  nil
  (rustic-mode))

;; Skeletons

(defmacro rust-skeleton-define (name doc &rest skel)
  "Define a `rust-mode' skeleton using NAME DOC and SKEL.
The skeleton will be bound to rust-skeleton-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
	 (funcname (intern (concat "rust-skeleton-" name))))
    `(progn
       (define-skeleton ,funcname
	 ,(or doc (format "Insert %s statement." name))
	 ,@skel))))

(rust-skeleton-define function "Insert a function definition."
  "Name: "
  "fn " str "(" ("Parameter, %s: "
		 (unless (equal ?\( (char-before)) ", ")
		 str) ") {" \n
  > "// code..." _ \n < "}" \n)

(rust-skeleton-define match "Insert a match expression."
  "Expression: "
  "match " str
  " {" ("Case Expression, %s: "
	\n str " => ...,")
  resume:
  ?\n > ?} (when (y-or-n-p "Insert \";\"? ") ?\;))

;; Keymaps

(define-key rust-mode-map (kbd "M-S-SPC") #'company-capf)
(define-key rust-mode-map (kbd "C-c i D") #'rust-ext-docstring)
(define-key rust-mode-map (kbd "C-c C-c M-c") #'rust-ext-finish-variable-type)

(define-prefix-command 'user-ext-rust-skeleton-map)
(define-key rust-mode-map (kbd "C-c C-t") #'user-ext-rust-skeleton-map)
(define-key user-ext-rust-skeleton-map (kbd "f") #'rust-skeleton-function)
(define-key user-ext-rust-skeleton-map (kbd "m") #'rust-skeleton-match)

(define-prefix-command 'user-ext-rust-motion-map)
(define-key rust-mode-map (kbd "C-c C-m") #'user-ext-rust-motion-map)
(define-key user-ext-rust-motion-map (kbd "f") #'rust-ext-forward-fn)
(define-key user-ext-rust-motion-map (kbd "M-f") #'rust-ext-backward-fn)
(define-key user-ext-rust-motion-map (kbd "i") #'rust-ext-forward-impl)
(define-key user-ext-rust-motion-map (kbd "M-i") #'rust-ext-backward-impl)

(define-key rust-mode-map (kbd "C-c C-j") #'imenu)

(easy-menu-define user-ext-rust-menu-map rustic-mode-map
  "Rustic Major Mode"
  '("Rustic"
    ("Cargo"
     ;; Project-level build/clean commands
     ["New Project" rustic-cargo-new]
     ["Initialize Project" rustic-cargo-init]
     "---"
     ["Build Project" rustic-cargo-build]
     ["Run Project" rustic-cargo-run]
     ["Run Project With Args" rust-ext-cargo-run-with-args]
     ["Check Project" rustic-cargo-check]
     ["Run Tests On Project" rustic-cargo-test]
     ["Run Benchmarks" rustic-cargo-bench]
     ["Clean Project" rustic-cargo-clean]
     "---"
     ["Add Crate" rustic-cargo-add]
     ["Remove Crate" rustic-cargo-rm]
     ["List Outdated Packages" rustic-cargo-outdated]
     ["Upgrade Dependencies" rustic-cargo-upgrade]
     "---"
     ["Open Documentation In Browser" rustic-cargo-doc]
     ["Run Test At Point" rustic-cargo-current-test]
     ["Format Code" rustic-cargo-fmt]
     ["Lint Project With Clippy" rustic-cargo-clippy]
     ["Fix With Clippy" rustic-cargo-clippy-fix]
     ["Run Clippy" rustic-cargo-clippy])
    ("Extensions"
     ["Write Documentation" rust-ext-docstring]
     ["Finish Variable Type" rust-ext-finish-variable-type]
     ("Skeletons"
      ["Function" rust-skeleton-function]
      ["Match Statement" rust-skeleton-match])
     ("Motion"
      ("Backward"
       ["Move Backward Function" rust-ext-backward-fn]
       ["Move Backward Implementation" rust-ext-backward-impl])
      ("Forward"
       ["Move Forward Function" rust-ext-forward-fn]
       ["Move Forward Implementation" rust-ext-forward-impl])))
    ["Compile Program" rustic-compile]
    ["Recompile Program" rustic-recompile]
    ["Format This Buffer" rustic-format-buffer]
    ["Make A Documentation" rustic-docstring-dwim]
    ["Customize This Group" rust-ext-customize-group]))

;;;###autoload
(defun rust--extra-hook ()
  "Hook for `rustic-mode' extension."
  (setq-local skeleton-further-elements
	      '((< '(- (min rust-indent-offset (current-column))))))
  (visual-line-mode t))

;;;###autoload
(add-hook 'rust-mode-hook #'rust--extra-hook)

(provide 'rust-ext)

;;; rust-ext ends here
