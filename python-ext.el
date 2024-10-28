;; -*- lexical-binding: t; -*-

;;; Code:

(require 'python-mode)
(require 'python)

(eval-when-compile
  (require 'debug-ext))

;; Variables

(defgroup python-ext nil
  "A group for Python extension."
  :group 'user-extensions)

(defcustom user-ext-python-docstring-fill-column 60
  "Fill column for docstring."
  :type 'integer
  :group 'python-ext)

(defcustom user-ext-python-docstring-major-mode 'markdown-mode
  "Major mode for editing Python docstring."
  :type 'symbol
  :group 'python-ext)

(defvar user-ext-python--orig-position nil
  "Position in the original buffer when editing Python docstring.")

(defvar user-ext-python--docstring-buffer nil
  "Buffer for Python docstring.")

;; Functions

;; (defmacro python-skeleton-define (name doc &rest skel)
;;   "Define a `python-mode' skeleton using NAME DOC and SKEL.
;; The skeleton will be bound to python-skeleton-NAME and will
;; be added to `python-mode-skeleton-abbrev-table'."
;;   (declare (indent 2))
;;   (let* ((name (symbol-name name))
;;          (function-name (intern (concat "python-skeleton-" name))))
;;     `(progn
;;        (define-abbrev python-mode-skeleton-abbrev-table
;;          ,name "" ',function-name :system t)
;;        (setq python-skeleton-available
;;              (cons ',function-name python-skeleton-available))
;;        (define-skeleton ,function-name
;;          ,(or doc
;;               (format "Insert %s statement." name))
;;          ,@skel))))

(define-scratch-buffer-function python-ext-scratch "python" nil
  "Opens a scratch buffer to let you write Python code."
  nil
  (python-mode))

(defun python-ext--write-docstring ()
  "Exit the Python docstring buffer and apply the docstring."
  (interactive)
  (let (num-lines prefix buffer-string)
    (deactivate-mark)
    (setq num-lines (count-lines (point-min) (point-max))) ; count number of lines
    ;; extract contents of buffer into a variable
    (with-current-buffer user-ext-python--docstring-buffer
      (setq buffer-string (buffer-string)))
    ;; exit function if string is empty
    (when (string-blank-p buffer-string)
      (kill-buffer)
      ;; (delete-window)
      (jump-to-register py--edit-register)
      (error "Docstring is empty"))
    ;; (kill-region (point-min) (point-max))
    (kill-buffer)
    ;; (delete-window)
    (jump-to-register py--edit-register)
    (insert buffer-string)
    (assert (markerp user-ext-python--orig-position))
    (goto-char user-ext-python--orig-position) ; go back to original position
    (save-excursion
      ;; get leading spaces
      (beginning-of-line)
      (setq prefix
	    (if (looking-at "^\\([ \t]+\\)")
		(match-string 1)
	      "")))
    ;; go to the beginning of each line and indent it
    ;; starting on the second line
    (move-beginning-of-line 2)
    (dotimes (_i (1- num-lines))
      (if (looking-at-p "[ \t]*\n")	; check if line is blank
	  (move-beginning-of-line 2)	; if blank, go to next line
	(insert prefix)			; otherwise, insert prefix
	(move-beginning-of-line 2)))
    (setq py--edit-register nil)
    (setq user-ext-python--orig-position nil)))

(defun python-ext-docstring ()
  "Open a temporary buffer to write a docstring."
  (interactive)
  (let ((mode user-ext-python-docstring-major-mode)
	modes)
    (window-configuration-to-register py--edit-register)
    (when abbrev-mode (push 'abbrev-mode modes))	 ; push minor modes from original buffer
    (when sphinx-doc-mode (push 'sphinx-doc-mode modes)) ;
    ;; save position in original buffer
    (setq user-ext-python--orig-position (point-marker))
    (setq user-ext-python--docstring-buffer (tmpbuf "python-docstring")) ; create a temporary buffer
    (assert (not (null user-ext-python--docstring-buffer)))		 ;
    (split-window-sensibly)				 ;
    (switch-to-buffer user-ext-python--docstring-buffer) ; Split window and enter temporary buffer
    (funcall mode)
    (dolist (mode modes)
      (princ `(funcall ,mode 1))
      (funcall mode 1))
    ;; set fill column and enable auto-fill
    (set-fill-column user-ext-python-docstring-fill-column)
    (auto-fill-mode 1)
    ;; set C-c C-c key to exit
    (local-set-key (kbd "C-c C-c") #'python-ext--write-docstring)
    (setq header-line-format "Python Docstring: Type C-c C-c to apply changes")
    (message "Type C-c C-c to save changes.")))

(defun python-ext-shift-return ()
  "Called when the user presses <S-return>."
  (interactive)
  (py-newline-and-indent)
  (py-newline-and-indent)
  (py-electric-backspace)
  (forward-line -1)
  (py-indent-or-complete))

;; Skeletons

;; Key bindings

(define-prefix-command 'python-ext-command-prefix nil "Python Ex")

(define-key python-mode-map (kbd "C-c i")      #'python-ext-command-prefix)
(define-key python-ext-command-prefix "D"      #'python-ext-docstring)

(define-key python-ext-command-prefix "c"      #'python-skeleton-class)
(define-key python-ext-command-prefix "d"      #'python-skeleton-def)
(define-key python-ext-command-prefix "f"      #'python-skeleton-for)
(define-key python-ext-command-prefix "I"      #'python-skeleton-if)
(define-key python-ext-command-prefix "m"      #'python-skeleton-import)
(define-key python-ext-command-prefix "T"      #'python-skeleton-try)
(define-key python-ext-command-prefix "w"      #'python-skeleton-while)

(define-key python-mode-map (kbd "<S-return>") #'python-ext-shift-return)
(define-key python-mode-map (kbd "M-e")        #'yas-expand)

;; company-capf
(define-key python-mode-map (kbd "M-SPC") #'company-capf)

;; Abbrevs

(define-abbrev python-mode-abbrev-table "rnone" "-> None" #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "rbool" "-> bool" #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "rself" "-> Self" #'abbrev-ext-insert-hook :system t)
(define-abbrev python-mode-abbrev-table "false" "False"   #'abbrev-ext-insert-hook :system t)

;; Hooks

(defun python--extra-hook ()
  "Hook for `python-mode' for this extension."
  (setq-local beginning-of-defun-function #'py-backward-def-or-class)
  (outline-minor-mode -1))

(defun python--lsp-hook ()
  "Hook for `python-mode' when lsp is enabled."
  (when (eq major-mode 'python-mode)
    (setq completion-at-point-functions
	  (delq #'py-fast-complete completion-at-point-functions))))

(add-hook 'python-mode-hook #'python--extra-hook)
(add-hook 'lsp-after-open-hook #'python--lsp-hook)
