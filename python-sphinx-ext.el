;; python-sphinx minor mode

(eval-when-compile
  (require 'sphinx-doc))

(defun sphinx-mode--extra-hook ()
  (define-key sphinx-doc-mode-map (kbd "C-c i d") #'sphinx-doc)
  (define-key sphinx-doc-mode-map (kbd "C-c M-r") #'sphinx-add-reference))

(add-hook 'sphinx-doc-mode-hook #'sphinx-mode--extra-hook)

;; (defun sphinx-add-keyword (type name &optional param)
;;   "Add a keyword argument at"
;;   (interactive "sType: \nsName:\nP"))

(defun sphinx-add-reference (&optional param)
  "Add a reference at point. If the prefix arg PARAM is non-nil, the alternative
form of the ref is inserted, if it has one."
  (interactive "P")
  (sphinx-add-reference--next-param param))

(defun sphinx-add-reference--next-param (param)
  (let (char)
    (setq char (read-char-from-minibuffer "a = attribute, c = class, e = exception, f = function, m = method, r = ref: "
					  '(?a ?c ?e ?f ?m ?r)))
    (cond
     ((= char ?c)
      ;; insert :py:class:`...`
      (sphinx--add-py "class" param))
     ((= char ?e)
      ;; insert :py:exc:`...`
      (sphinx--add-py "exc" param))
     ((= char ?f)
      ;; insert :py:func:`...`
      (sphinx--add-py "func" param))
     ((= char ?m)
      ;; insert :py:meth:`...`
      (sphinx--add-py "meth" param))
     ((= char ?a)
      ;; insert :py:attr:`...`
      (sphinx--add-py "attr" param))
     ((= char ?r)
      ;; insert :ref:`...`
      (if current-prefix-arg
	  (sphinx--add-ref-with-link)
	(sphinx--add-ref)))
     (t
      (error (format "invalid character %c" char))))))

(defun sphinx--add-ref ()
  (let ((label (read-from-minibuffer "Label: "))
	(buf (current-buffer)))
    (princ (format ":ref:`%s`" label) buf)))

(defun sphinx--add-ref-with-link ()
  (let ((label (read-from-minibuffer "Label: "))
	(link (read-from-minibuffer "Link: "))
	(title (read-from-minibuffer "Title: "))
	(buf (current-buffer)))
    (princ (format ":ref:`%s %s <%s>`" link title label) buf)))

(defun sphinx--add-py (field &optional param)
  (let ((rolename (format ":py:%s:" field))
	(ref (read-from-minibuffer "Ref: "))
	(buf (current-buffer)))
    (princ (format "%s`%s`" rolename ref) buf)))
