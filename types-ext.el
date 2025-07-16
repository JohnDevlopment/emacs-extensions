;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'cl-ext)
  (require 'embed-doc)

  (embed-doc-document-symbol types-ext
    "Custom types.

Types:
- `list-or-null'
- `marker-or-null'
- `integer-or-null'
- `string-or-null'
- `unsigned-byte'
- `seconds-type'
- `alist'"
    :functions
    alist-p
    alist-of))

(defun alist-p (obj)
  "Return t if OBJ is a valid alist."
  (and (listp obj)
       (cl-every (lambda (elt)
		   (and (consp elt)
			(not (null (car elt)))
			(not (consp (car elt)))))
		 obj)))

(defun alist-of (obj key-type value-type)
  "Return t if OBJ is a valid alist."
  (cl-ext-when (listp obj)
      (cl-every (lambda (elt &optional k v)
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

(provide 'types-ext)
;;; types-ext.el ends here
