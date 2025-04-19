;; -*- lexical-binding: t; -*-

(require 'ert)

(eval-when-compile
  (require 'cl-lib))

;; ### Functions

;;;###autoload
(defun alist-ext-from-list (list)
  "Turn a regular list LIST into an alist.
LIST must have an even number of elements."
  (declare (pure t))
  (unless (= (mod (length list) 2) 0)
    (user-error "Must have even number of arguments."))
  (seq-partition list 2))

;;;###autoload
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
      (cl-ext-append-list (list 'cons key value) res))
    `(list ,@res)))


;; ### Tests

(ert-deftest alist-ext-test-define ()
  "Tests the result of `alist-ext-define'."
  (let ((al (alist-ext-define 'a 1 'b 2)))
    (should (equal al '((a . 1) (b . 2))))))

(provide 'alist-ext)
;;; alist-ext.el ends here
