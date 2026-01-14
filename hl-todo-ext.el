;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")
(extension-check-requires hl-todo)

(require 'hl-todo)


;; ### Customization

(defgroup hl-todo-ext nil
  "Hl-todo extension."
  :group 'user-extensions)

(defun user-ext-hl-todo-command-prefix--setter (symbol value)
  (set-default-toplevel-value symbol value)
  (define-prefix-command 'user-ext-hl-todo-command-map)
  (set-keymap hl-todo-mode-map value 'user-ext-hl-todo-command-map)
  (set-keymap user-ext-hl-todo-command-map "o" #'hl-todo-occur)
  (set-keymap user-ext-hl-todo-command-map "p" #'hl-todo-previous)
  (set-keymap user-ext-hl-todo-command-map "n" #'hl-todo-next))
(--ignore
  (user-ext-hl-todo-command-prefix--setter
   'user-ext-hl-todo-command-prefix
   user-ext-hl-todo-command-prefix)

  (set-keymap hl-todo-mode-map "n" nil)

  (setq hl-todo-map (make-sparse-keymap))

  (prog1 nil
    (with-current-buffer (get-buffer-create "*output*")
      (emacs-lisp-mode)
      (cl-prettyprint (symbol-function #'user-ext-hl-todo-command-prefix--setter))
      (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
      (set-buffer-modified-p nil))
    (pop-to-buffer "*output*" t)
    (call-interactively #'menu-bar--toggle-truncate-long-lines))
  t)
(defcustom user-ext-hl-todo-command-prefix "C-c #"
  "Command prefix."
  :type 'key
  :set #'user-ext-hl-todo-command-prefix--setter
  :require 'hl-todo
  :group 'hl-todo-ext)


;; ### Hooks

;;;###autoload
(defun hl-todo--extra-hook () t)

;;;###autoload
(add-hook 'hl-todo-mode-hook #'hl-todo--extra-hook)


(extension-provide 'hl-todo-ext)
;;; hl-todo-ext.el ends here
