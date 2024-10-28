;;; liquidsoap-ext --- Liquidsoap mode extension.  -*- lexical-binding: t;  -*-

;;; Commentary:

;;; Code:

(require 'liquidsoap-mode)
(require 'liquidsoap-completions)

(require 'debug-ext)

(defgroup user-ext-liquidsoap-mode nil
  "A group for `liquidsoap-mode'."
  :group 'user-extensions)

;;; Customization

(defface user-ext-liquidsoap-help-arg
  '((t (:inherit font-lock-variable-name-face)))
  "A face for arguments in a `liquidsoap-mode' help buffer."
  :group 'user-ext-liquidsoap-mode)

(defface user-ext-liquidsoap-help-type
  '((t (:inherit font-lock-type-face)))
  "A face for argument types in a `liquidsoap-mode' help buffer."
  :group 'user-ext-liquidsoap-mode)

(defcustom user-ext-liquidsoap-ext-binary-path ""
  "Path to the liquidsoap binary."
  :type '(file :must-match t)
  :group 'user-ext-liquidsoap-mode)

;;; Variables/constants

(defvar user-ext-liquidsoap-ext-doc-cache nil
  "An alist of commands mapped to their docstrings.")

(defconst user-ext-liquidsoap-ext-docheader-regexp
  "\\(Arguments\\|Category\\|Methods\\|Type\\):"
  "Regular expression matching the beginning of a documentation section.")

;; (defun liquidsoap-ext--get-doc (command)
;;   "Get documentation for COMMAND."
;;   (list (get-text-property 0 :description command)
;; 	(get-text-property 0 :type command)))

(defun liquidsoap-ext--cache-get-doc (command)
  "Return the cached documentation of COMMAND.

This function returns the element COMMAND from the alist at
`liquidsoap-ext-doc-cache'.  The result is a cons with
COMMAND and its associated value.  If COMMAND is not in the
cache, this returns nil."
  (assert (listp user-ext-liquidsoap-ext-doc-cache))
  (assoc command user-ext-liquidsoap-ext-doc-cache #'string=))

(defun liquidsoap-ext--cache-add-doc (command doc)
  "Add a mapping of COMMAND to its documentation DOC.
This function modifies `liquidsoap-ext-doc-cache'."
  (assert (listp user-ext-liquidsoap-ext-doc-cache))
  (cl-pushnew (cons command doc) user-ext-liquidsoap-ext-doc-cache))

(defun liquidsoap-ext--get-doc (command)
  "Get documentation for COMMAND."
  (let ((doc (liquidsoap-ext--cache-get-doc command))
	result)
    ;; Unless the documentation is cached, call a subprocess
    (unless doc
      (call-process user-ext-liquidsoap-ext-binary-path	  ; Call subprocess to get documentation
		    nil "*Subprocess Output*" nil	  ; and insert it into a buffer
		    "-h" command)
      (with-current-buffer "*Subprocess Output*"
	(setq doc (buffer-string) result doc) ; Get the string from the buffer
	(assert (stringp result)))	      ; Make sure result is a string
      (kill-buffer "*Subprocess Output*")
      ;; Add documentation to cache
      (liquidsoap-ext--cache-add-doc command doc))
    (if (listp doc)
	(cdr doc)
      doc)))

(defun liquidsoap-ext--complete-command ()
  "Get a command from `liquidsoap-completions' via completion."
  (let* ((command (completing-read "Command: " liquidsoap-completions))
	 (result (cl-find command liquidsoap-completions :test 'string=)))
    (assert (not (null result)))
    (assert (stringp result))
    (list result)))

;; (defun liquid-soap-ext--get-block-region (type)
;;   )

;; (defun liquid-soap-ext--format-doc-buffer-arguments (buffer beg end)
;;   "Go into BUFFER and add text properties inside the region BEG and END."
;;   )

(defun liquid-soap-ext--format-doc-buffer (buffer)
  "Go into BUFFER and add text properties.
BUFFER is a buffer containing Liquidsoap documentation."
  (let (line pm beg end)
    (with-current-buffer buffer
      (goto-char 1)
      (save-match-data
	;; Skip the first line
	(forward-line 1)
	(setq pm (point-marker))
	;; Loop until end of buffer
	(while (not (eobp))
	  (setq line (thing-at-point 'line)) ; get the current line
	  (if (looking-at user-ext-liquidsoap-ext-docheader-regexp)
	      ;; Header, make it bold
	      (put-text-property (match-beginning 1) (match-end 1) 'face 'bold))
	  (forward-line 1)
	  (goto-char 1))))))

;; (defun liquidsoap-ext-get-doc (command)
;;   (interactive (liquidsoap-ext--complete-command))
;;   (with-help-window (help-buffer)
;;     (cl-destructuring-bind (doc type) (liquidsoap-ext--get-doc command)
;;       (princ (format "%s%s\n\n%s" command type doc)))
;;     (help-setup-xref (list 'liquidsoap-ext-get-doc command)
;; 		     (called-interactively-p 'interactive))
;;     (with-current-buffer (help-buffer)
;;       (let ((arg-regexp "\\??\\([a-z_]+?\\) :")
;; 	    (arg-regexp-type "[a-z_]+? : \\([a-z_]+\\)\\??,?")
;; 	    (arg-regexp-return-type " -> \\([a-z_]+\\)"))
;; 	(highlight-regexp arg-regexp 'liquidsoap-help-arg 1)
;; 	(highlight-regexp arg-regexp-type 'liquidsoap-help-type 1)
;; 	(highlight-regexp arg-regexp-return-type 'liquidsoap-help-type 1)))))

;;;###autoload
(defun liquidsoap-ext-get-doc (command)
  "Display a help window for the Liquidsoap COMMAND.

Interactively, the user is prompted for a Liquidsoap
command, with completion."
  (interactive (liquidsoap-ext--complete-command))
  (let ((doc (liquidsoap-ext--get-doc command)))
    (with-help-window (help-buffer)
      (princ doc)
      (help-setup-xref (list 'liquidsoap-ext-get-doc command)
		       (called-interactively-p 'interactive)))))

(define-key liquidsoap-mode-map (kbd "C-c h") #'liquidsoap-ext-get-doc)

;;;###autoload
(defun liquidsoap-ext--hook ()
  "Extra hook for `liquidsoap-mode'."
  (enable-wrap))

;;;###autoload
(add-hook 'liquidsoap-mode-hook #'liquidsoap-ext--hook)

(provide 'liquidsoap-ext)

;;; liquidsoap-ext ends here
