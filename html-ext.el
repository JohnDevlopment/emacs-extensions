;;; html-ext --- HTML mode extension.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'alist-ext)
(require 'mhtml-mode)
(require 'ido)
(require 'sgml-mode)

(defconst user-ext-mhtml-entities
  (alist-ext-define "QUOTATION MARK (\")" "&quot;"
		    "AMPERSAND (&)" "&amp;"
		    "LESS-THAN SIGN (<)" "&lt;"
		    "GREATER-THAN SIGN (>)" "&gt;"
		    "NO-BREAK SPACE" "&nbsp;"
		    "CENT SIGN ¢" "&cent;"
		    "POUND SIGN (£)" "&pound;"
		    "CURRENCY SIGN (¤)" "&curren;"
		    "YEN SIGN (¥)" "&yen;"
		    "BROKEN BAR (¦)" "&brvbar;"
		    "SECTION SIGN (§)" "&sect;"
		    "COPYRIGHT SIGN (©)" "&copy;"
		    "LEFT-POINTING DOUBLE ANGLE QUOTATION MARK («)" "&laquo;"
		    "SOFT HYPHEN" "&shy;"
		    "REGISTERED SIGN (®)" "&reg;"
		    "MACRON (¯)" "&macr;"
		    "DEGREE SIGN (°)" "&deg;"
		    "PLUS-MINUS SIGN (±)" "&plusmn;"
		    "BALLOT BOX WITH CHECK (☑)" "&#9745;"
		    "BALLOT BOX WITH X (☒)" "&#9746;"))

(define-key mhtml-mode-map (kbd "C-M-i") #'completion-at-point)
(define-key mhtml-mode-map (kbd "C-c C-e") #'mhtml-ext-insert-entity)

(defun mhtml-ext--entities ()
  (cl-loop
   for i below (length user-ext-mhtml-entities)
   collect (car (nth i user-ext-mhtml-entities))))

;;;###autoload
(defun mhtml-ext-insert-entity (name)
  "Insert an entity symbol called NAME."
  (interactive (list (ido-completing-read "Name: " (mhtml-ext--entities))))
  (let ((entity (alist-get name user-ext-mhtml-entities)))
    (insert entity)))

;;;###autoload
(defun mhtml--extra-hook () t)

;;;###autoload
(add-hook 'mhtml-mode-hook #'mhtml--extra-hook)

(provide 'html-ext)

;;; html-ext ends here
