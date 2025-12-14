;; -*- lexical-binding: t; -*-

(require 'custom)
(require 'font-lock)

(eval-when-compile
  (require 'debug-ext))

;; ### Customization

(defgroup erd nil
  "Major mode for editing entity-relation diagrams."
  :group 'languages
  :package-version "20250816")
(--ignore
 (--symbol-plist 'erd)
 t)

(defface erd-entity-face
  '((t (:inherit font-lock-constant-face)))
  "Face for entities."
  :group 'erd)

;; ### Variables

(defmacro erd-rx (&rest regexps)
  "Erd variation of `rx'.
\(fn REGEXP...)"
  (rx-let ()
    `(rx ,@regexps)))

(defconst erd-entity-regexp
  (erd-rx bol ?\[ (* nonl) ?\] eol)
  "Regular expression for entities.")

(defconst erd-entity-face 'erd-entity-face
  "Variable alias for face `erd-entity-face'.")

(defvar erd-font-lock-keywords-1 nil
  "Keywords for level 1 font lock.

Level 1 entails:
- Preprocessor directives
- Strings
- Comments
- Function declarations")

(defvar erd-font-lock-keywords-2 nil
  "Keywords for level 2 font lock.

Level 2 entails:
- Everything from level 1
- Lanugage keywords
- Constants
- Types")

(defvar erd-font-lock-keywords-3
  "Keywords for level 3 font lock.")

(defvar erd-font-lock-keywords nil
  "Mode-default font lock keywords.
See `font-lock-keywords'.")

;; ### Functions

(defun erd--setup-font-lock ()
  (setq erd-font-lock-keywords-1 nil
	erd-font-lock-keywords-2
	(append erd-font-lock-keywords-1
		`((,erd-entity-regexp . (0 erd-entity-face))))
	erd-font-lock-keywords-3 erd-font-lock-keywords-2
	erd-font-lock-keywords erd-font-lock-keywords-1))

;; ### Major mode

;;;###autoload
(define-derived-mode erd-mode prog-mode
  "Erd"
  "Major mode for editing entity-relation diagrams."
  :group 'erd
  ;; :after-hook (erd-initialize-font-lock)
  (setq font-lock-defaults
	'((erd-font-lock-keywords
	   erd-font-lock-keywords-1
	   erd-font-lock-keywords-2
	   erd-font-lock-keywords-3))))
(--ignore
 (--symbol-plist 'erd-mode)
 t)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.er\\'" . erd-mode))

(--ignore
 (font-lock-add-keywords
  nil
  `((,code-outline-comment-line-regexp . 'code-outline-comment-line)
    (,code-outline-variable-regexp . (0 'code-outline-variable t))
    (,code-outline-constant-regexp . 'code-outline-constant)
    (,code-outline-section-header-regexp . (1 'code-outline-section-header))
    (,code-outline-pre-regexp . 'code-outline-inline-code)
    (,code-outline-comment-line-regexp . 'code-outline-comment)
    ("`\\(.*?\\)'" . 'code-outline-inline-code)))
 t)

(provide 'erd-mode)
;;; erd-mode.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; End:
