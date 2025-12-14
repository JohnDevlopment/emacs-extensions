;; -*- lexical-binding: t; -*-


;; ### Customization

(defgroup magit-ext nil
  "Extension to Magit."
  :group 'user-extensions)

(defcustom user-ext-magit-auto-enable-filenotify nil
  "If non-nil, auto-enable `magit-filenotify-mode'."
  :type 'boolean
  :group 'magit-ext)


;; ### Variables

(defvar-local user-ext-magit--timer nil)


;; ### Hooks

(defun magit-status--extra-hook ()
  (when (and (fboundp #'magit-filenotify-mode)
	     user-ext-magit-auto-enable-filenotify)
    (setq user-ext-magit--timer
	  (run-with-idle-timer
	   1 nil
	   (lambda ()
	     (kill-local-variable 'user-ext-magit--timer)
	     (magit-filenotify-mode 1))))))

(add-hook 'magit-status-mode-hook #'magit-status--extra-hook)

(provide 'magit-ext)
;;; magit-ext.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "mx" "magit-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-magit")
;; End:
