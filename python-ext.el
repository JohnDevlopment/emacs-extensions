(eval-and-compile
  (require 'python-mode))

(eval-when-compile
  (require 'python))

(defcustom python-ext-docstring-fill-column 60
  "Fill column for docstring."
  :type 'integer
  :group 'user-extensions)

;; (defcustom python-ext-docstring-indent-offset 4
;;   "Indentation offset for python."
;;   :type 'integer
;;   :group 'user-extensions)

;;; Functions

(defun python-ext-docstring-ctr-c-ctr-c ()
  "Exit the Python docstring buffer and apply the changes."
  (interactive)
  (if (use-region-p)
      (deactivate-mark))
  (kill-region (point-min) (point-max))
  (kill-buffer)
  (delete-window)
  (yank))

(defun python-ext-docstring ()
  "Open a temporary buffer to write a docstring."
  (interactive)
  (split-window-sensibly)
  (tmpbuf "python-docstring" t)
  (indented-text-mode)
  (set-fill-column python-ext-docstring-fill-column)
  (auto-fill-mode)
  (setq-local indent-tabs-mode nil)
  (local-set-key (kbd "C-c C-c") #'python-ext-docstring-ctr-c-ctr-c))

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

;; Key bindings

(eval-and-compile
  (define-prefix-command 'python-ext-command-prefix nil "Python Ex")
  (define-key python-mode-map (kbd "C-c i") #'python-ext-command-prefix)
  (define-key python-ext-command-prefix "D" #'python-ext-docstring)
  (define-key python-ext-command-prefix "c" #'python-skeleton-class)
  (define-key python-ext-command-prefix "d" #'python-skeleton-def)
  (define-key python-ext-command-prefix "f" #'python-skeleton-for)
  (define-key python-ext-command-prefix "I" #'python-skeleton-if)
  (define-key python-ext-command-prefix "m" #'python-skeleton-import)
  (define-key python-ext-command-prefix "T" #'python-skeleton-try)
  (define-key python-ext-command-prefix "w" #'python-skeleton-while)
  (define-key python-mode-map (kbd "<S-return>") #'python-ext-shift-return)
  (define-key python-mode-map (kbd "M-e") #'yas-expand))

;;; Hook

(defun python--extra-hook ()
  (setq-local beginning-of-defun-function #'py-backward-def-or-class)
  (electric-pair-local-mode)
  (outline-minor-mode -1))

(add-hook 'python-mode-hook #'python--extra-hook)
