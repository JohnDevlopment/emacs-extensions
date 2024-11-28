;;; rust-ext --- Rust mode extension.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

(defvar user-ext-rust--register ?r)

(defvar user-ext-rust--docstring-buffer nil
  "Docstring buffer in `rust-mode'.")

;; Functions

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
    (assert (not (null user-ext-rust--docstring-buffer)))
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
      ["Match Statement" rust-skeleton-match]))
    ["Compile Program" rustic-compile]
    ["Recompile Program" rustic-recompile]
    ["Format This Buffer" rustic-format-buffer]
    ["Make A Documentation" rustic-docstring-dwim]
    ["Customize This Group" rust-ext-customize-group]))

;;;###autoload
(defun rust--custom-hook ()
  "Hook for `rustic-mode' extension."
  (setq-local skeleton-further-elements
	      '((< '(- (min rust-indent-offset (current-column))))))
  (visual-line-mode t))

;;;###autoload
(add-hook 'rust-mode-hook #'rust--custom-hook)

(provide 'rust-ext)

;;; rust-ext ends here
