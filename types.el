;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(eval-when-compile
  (require 'cl-lib)
  (require 'cl-ext)
  (require 'embed-doc))

(require 'dash)

(eval-and-compile
  (eval `(embed-doc-document-symbol types
	   "Custom types."
	   :types
	   list-or-null
	   marker-or-null
	   integer-or-null
	   string-or-null
	   unsigned-byte
	   seconds-type
	   alist
	   keymap
	   ,(with-emacs-version >= "29.1" 'key)
	   :functions
	   alist-p
	   alist-of)))

(defun alist-p (obj)
  "Return t if OBJ is a valid alist."
  (and (listp obj)
       (-every (lambda (elt)
		 (and (consp elt)
		      (not (null (car elt)))
		      (not (consp (car elt)))))
	       obj)))

(defun alist-of (obj key-type value-type)
  "Return t if OBJ is a valid alist."
  (when (listp obj)
    (-every (lambda (elt &optional k v)
	      (and (consp elt)
		   (setq k (car elt))
		   (not (consp (setq v (car elt))))
		   (cl-typep k key-type)
		   (cl-typep v value-type)))
	    obj)))

(cl-deftype list-or-null () '(or list null))
(cl-deftype marker-or-null () '(or marker null))
(cl-deftype integer-or-null () '(or integer null))
(cl-deftype string-or-null () '(or string null))
(cl-deftype unsigned-byte (&optional bits)
  `(integer 0 ,(if (eq bits '*) bits (1- (ash 1 bits)))))
(cl-deftype seconds-type () '(or (integer 1 *) (float 0.01 *)))
(cl-deftype alist () '(satisfies alist-p))
(cl-deftype keymap () '(satisfies keymapp))
(with-emacs-version >= "29.1"
  (cl-deftype key () '(satisfies key-valid-p)))

(extension-provide 'types-ext)
;;; types-ext.el ends here
