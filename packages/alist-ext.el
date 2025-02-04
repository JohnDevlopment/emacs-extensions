;;; alist-ext.el --- Functions for alists            -*- lexical-binding: t; -*-

(require 'debug-ext)
(require 'cl-lib)

(defun alist-ext-from-list (list)
  "Turn a regular list LIST into an alist.
LIST must have an even number of elements."
  (declare (pure t))
  (unless (= (mod (length list) 2) 0)
    (user-error "Must have even number of arguments."))
  (seq-partition list 2))

(defmacro alist-ext-define (&rest pairs)
  "Construct an alist with PAIRS, where each key precedes its value.

Each key can be any valid lisp object, but symbols have to
be quoted.

\(fn KEY VALUE ...)"
  (unless (= (mod (length pairs) 2) 0)
    (error "alist-ext-define requires an even number of arguments (key-value pairs)."))
  (let (res key value)
    (while pairs
      (setq key (pop pairs) value (pop pairs))
      (cl-append-list (list 'cons key value) res))
    `(list ,@res)))

(provide 'alist-ext)

;;; alist-ext.el ends here
