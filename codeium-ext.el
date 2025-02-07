;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/codeium.el")

(require 'codeium)
(require 'easymenu)

;; Variables

(defvar user-ext-codeium-menu-map nil
  "Keymap for Codeium menu.")

(defvar user-ext-codeium-enabled nil
  "Non-nil if Codeium is enabled in buffer.")

(make-local-variable 'user-ext-codeium-enabled)

(defvar user-ext-codeium--is-init nil
  "Non-nil if Codeium is initialized in buffer.")

(make-local-variable 'user-ext-codeium--is-init)

;; Menu

(easy-menu-define user-ext-codeium-menu-map nil
  "Codeium commands"
  '("Codeium"
    ["Init" user-ext-codeium-init
     :enable (and (user-ext-codeium-installed-p)
		  (not (user-ext-codeium-init-p)))
     :help "Initialize Codeium."]
    ["Enable Codeium" user-ext-enable-codeium-completion
     :enable (and (user-ext-codeium-installed-p)
		  (not user-ext-codeium-enabled)
		  (user-ext-codeium-init-p))
     :help "Enable Codeium completion in the current buffer."]
    ["Disable Codeium" user-ext-disable-codeium-completion
     :enable (and (user-ext-codeium-installed-p)
		  user-ext-codeium-enabled)
     :help "Disable Codeium completion in the current buffer."]))

(define-key-after (lookup-key global-map [menu-bar])
  [codeium]
  (cons "Codeium" user-ext-codeium-menu-map)
  'tools)

;; Functions

(defun user-ext-codeium-init-p ()
  "Check if Codeium is initialized."
  (and (user-ext-codeium-installed-p) user-ext-codeium--is-init))

(defun user-ext-codeium-installed-p ()
  "Check if codeium is installed."
  (and (locate-library "codeium") t))

(defun user-ext-codeium-init ()
  "Initialize Codeium."
  (interactive)
  (when (user-ext-codeium-installed-p)
    (codeium-init)
    (setq user-ext-codeium--is-init t)
    (message "Codeium initialized.")))

(defun user-ext-disable-codeium-completion ()
  "Disable codeium completion in buffer."
  (interactive)
  (setq-local completion-at-point-functions
	      (cl-remove #'codeium-completion-at-point
			 completion-at-point-functions))
  (setq-local user-ext-codeium-enabled nil)
  (message "Codeium completion disabled in buffer."))

(defun user-ext-enable-codeium-completion ()
  "Enable codeium completion in buffer."
  (interactive)
  (cl-pushnew #'codeium-completion-at-point completion-at-point-functions)
  (setq-local user-ext-codeium-enabled t)
  (message "Codeium completion enabled in buffer."))

(provide 'codeium-ext)

;;; codeium-ext ends here
