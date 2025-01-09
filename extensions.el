;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'package))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defconst user-ext-extension-directory "~/.emacs.d/extensions")

(add-to-list 'load-path "~/.emacs.d/extensions")
(add-to-list 'load-path "~/.emacs.d/extensions/packages")

(setq read-process-output-max 10485760)

(setq frame-title-format
      (concat multiple-frames " %b " invocation-name "@" (system-name)))

;; Enable `narrow-to-region'
(put 'narrow-to-region 'disabled nil)

(defun --extension-completion (&optional initial-input)
  (let* (completion-ignored-extensions
	 (path "~/.emacs.d/extensions"))
    (list (completing-read "Load extension: "
			   (apply-partially 'locate-file-completion-table
					    (list path)
					    (get-load-suffixes))
			   nil nil initial-input))))

(defun load-extension (extension &optional noerror nomessage)
  "Load an extension.

EXTENSION is a file '~/.emacs.d/extensions' without its file
extension.  For example, with an extension named 'general',
the file '~/.emacs.d/extensions/general.el' is loaded.

Optional args NOERROR and NOMESSAGE are forwarded to `load'."
  (interactive (--extension-completion))
  (load (concat "~/.emacs.d/extensions/" extension) noerror nomessage))

(defun find-extension (extension)
  "Find the Emacs Lisp source of EXTENSION."
  (interactive (--extension-completion))
  (prog1
      (switch-to-buffer (find-file-noselect
			 (concat "~/.emacs.d/extensions/" extension ".el")))))

(defun find-extension-at-point (extension)
  (interactive
   (let* ((thing (thing-at-point 'sexp))
	  (end (if (stringp thing) (length thing))))
     (set-text-properties 0 end nil thing)
     (--extension-completion (when (stringp thing) thing))))
  (prog1
      (switch-to-buffer (find-file-noselect
			 (concat "~/.emacs.d/extensions/" extension ".el")))))

;; (desktop+-default-bindings)

;; Autoloads
(load-extension "loaddefs-ext" t)

;; (load-extension "abbrev-ext")
;; (load-extension "codeium-ext")
(load-extension "custom-ext")

(load-extension "general")
(load-extension "keymaps-ext")
(load-extension "macro-ext")

(load-extension "lsp-ext")
(load-extension "buffers-ext")
(load-extension "imenu-ext")
;; (load-extension "syntax-ext")

;; mode extensions
(load-extension "modes")
