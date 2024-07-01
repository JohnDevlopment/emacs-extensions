;;; python-ext.el --- Python extension -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'python-mode))

(eval-when-compile
  (require 'python)
  (require 'debug-ext))

(defcustom python-ext-docstring-fill-column 60
  "Fill column for docstring."
  :type 'integer
  :group 'user-extensions)

;; (defcustom python-ext-docstring-indent-offset 4
;;   "Indentation offset for python."
;;   :type 'integer
;;   :group 'user-extensions)
(defvar user-ext-python--orig-position nil
  "Position in the original buffer when editing Python docstring.")

(defvar user-ext-python--docstring-buffer nil
  "Buffer for Python docstring.")

;;; Functions

(defun python-ext--write-docstring ()
  "Exit the Python docstring buffer and apply the changes."
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
    (dotimes (i (1- num-lines))
      (if (looking-at-p "[ \t]*\n")	; check if line is blank
	  (move-beginning-of-line 2)	; if blank, go to next line
	(insert prefix)			; otherwise, insert prefix
	(move-beginning-of-line 2)))
    (setq py--edit-register nil)
    (setq user-ext-python--orig-position nil)))

(defun python-ext-docstring ()
  "Open a temporary buffer to write a docstring."
  (interactive)
  (window-configuration-to-register py--edit-register)
  ;; save position in original buffer
  (setq user-ext-python--orig-position (point-marker))
  ;; create a temporary buffer
  (setq user-ext-python--docstring-buffer (tmpbuf "python-docstring"))
  (assert (not (null user-ext-python--docstring-buffer)))
  ;; Split window and enter temporary buffer
  (split-window-sensibly)
  (switch-to-buffer user-ext-python--docstring-buffer)
  (markdown-mode)
  ;; set fill column and enable auto-fill
  (set-fill-column python-ext-docstring-fill-column)
  (auto-fill-mode 1)
  ;; set C-c C-c key to exit
  (local-set-key (kbd "C-c C-c") #'python-ext--write-docstring)
  (setq header-line-format "Python Docstring: Type C-c C-c to apply changes")
  (message "Type C-c C-c to save changes."))

(defun python-ext-scratch ()
  "Opens a scratch buffer to let you write Python code."
  (interactive)
  (tmpbuf "python")
  (python-mode))

(defun python-ext-shift-return ()
  "Called when the user presses <S-return>."
  (interactive)
  (py-newline-and-indent)
  (py-newline-and-indent)
  (py-electric-backspace)
  (forward-line -1)
  (py-indent-or-complete))

;;; Key bindings

(eval-and-compile
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
  (define-key python-mode-map (kbd "M-SPC") #'company-capf))

;;; Hook

(defun python--extra-hook ()
  (setq-local beginning-of-defun-function #'py-backward-def-or-class)
  (electric-pair-local-mode)
  (outline-minor-mode -1))

(add-hook 'python-mode-hook #'python--extra-hook)
