;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'f))

(check-emacs-minimum-version "27.4")

(defvar user-ext-hack--tree-sitter nil
  "Non-nil when `hack-tree-sitter-ext-no-eln' has been called.")

;; HACK: Delete tree-sitter-ext eln cache
;;;###autoload
(defun hack-tree-sitter-ext-no-eln ()
  "Delete tree-sitter-ext eln cache."
  (interactive)
  (unless user-ext-hack--tree-sitter
    (let ((sfile "~/.emacs.d/extensions/tree-sitter-ext.el")
	  (elc-file "~/.emacs.d/extensions/tree-sitter-ext.elc")
	  (eln-file (car-safe (f-glob "tree-sitter-ext-*.eln"
				      "~/.emacs.d/eln-cache/29.4-13c1224a/"))))
      (when (and eln-file (f-exists-p eln-file))
	(delete-file eln-file)
	(message "Deleted %s" eln-file))
      (when (f-exists-p elc-file)
	(delete-file elc-file)
	(message "Deleted %s" elc-file))
      (load-extension-safe "tree-sitter-ext")
      (unless (get-buffer "tree-sitter-ext.el")
	(find-extension "tree-sitter-ext"))
      (message "Compiling after 2 seconds")
      (setq user-ext-hack--tree-sitter t)
      (run-with-idle-timer 2 nil
			   (lambda ()
			     (emacs-lisp-byte-compile-and-load)
			     (sleep-for 2)
			     (if (fboundp 'jdesktop-clear)
				 (with-suppressed-warnings ((unresolved jdesktop-clear))
				   (jdesktop-clear))
			       (desktop-clear)))))))

(extension-provide 'hack)
;;; hack.el ends here
