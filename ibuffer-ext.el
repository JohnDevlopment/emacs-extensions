;; -*- lexical-binding: t; -*-

(check-emacs-minimum-version "27.1")

(require 'ibuffer)

(eval-when-compile
  (require 'alist-ext)
  (require 'function-ext))

(eval-and-compile
  (embed-doc-document-symbol ibuffer-ext
    "IBuffer extension."
    :commands
    ibuffer-ext-buffer-name
    ibuffer-ext-do-change-major-mode
    ibuffer-ext-insert-independent-filter-group
    ibuffer-ext-toggle-current-filter-group))


;; ### Customization

(defgroup ibuffer-ext nil
  "IBuffer extension."
  :group 'user-extensions)

(defcustom user-ext-ibuffer-global-filter-groups
  nil
  "A list of filter groups which can inserted independently.
Each element has the form (\"LABEL\" FILTER-SPECS...), where
FILTER-SPECS takes the form allowed in `ibuffer-filtering-qualifiers',
which see."
  :group 'ibuffer-ext
  :type '(repeat (list :value ("" nil)
		       (string :tag "Filter Group")
		       ibuffer-filter-spec)))


;; --- Ibuffer Filter Spec

(define-widget 'ibuffer-filter-spec 'lazy
  "A filter spec."
  :tag "Filter Spec"
  :offset 4
  :match #'widget-ibuffer-filter-spec-match
  :type '(choice (sexp :tag "Single Spec")
		 (list :tag "Not"
		       :value (not nil)
		       (const not) ibuffer-filter-spec)
		 (list :tag "And"
		       :value (and nil)
		       (const and) (repeat ibuffer-filter-spec))
		 (list :tag "Or"
		       :value (or nil)
		       (const or) (repeat ibuffer-filter-spec))
		 (cons :tag "Saved Filter"
		       (const saved) string)))

(defun widget-ibuffer-filter-spec-match (widget value) t)


;; ### Advice

(defun ibuffer-ext--after-operation (&rest _r)
  "Used as :after advice for operations."
  (ibuffer-unmark-all-marks))

(advice-add #'ibuffer-do-revert :after #'ibuffer-ext--after-operation)
(advice-add #'ibuffer-do-eval :after #'ibuffer-ext--after-operation)

(fext-defadvice ibuffer-do-view (after ibuffer-do-view)
  "Activate View mode after visiting buffer."
  (interactive)
  (view-mode 1))

(fext-defadvice ibuffer-visit-buffer (override ibuffer-visit-buffer (&optional single))
  "Visit the buffer on this line.
If optional argument SINGLE is non-nil, then also ensure
there is only one window.

When called interactively, SINGLE is the prefix argument."
  (interactive "P")
  (let ((buf (ibuffer-current-buffer t)))
    (bury-buffer)
    (switch-to-buffer buf)
    (when single
      (delete-other-windows))))


;; ### Functions

(defun ibuffer-ext-buffer-name ()
  "Display the name of the buffer on this line."
  (declare (interactive-only t))
  (interactive)
  (when-let ((buf (ibuffer-current-buffer t)))
    (message "Buffer: %s" (buffer-name buf))))

(defun ibuffer-ext-do-change-major-mode (mode)
  "Change the major mode of marked buffers to MODE."
  (interactive (list (general-scratch--complete-mode)))
  (let (buffers)
    (save-current-buffer
      (ibuffer-map-marked-lines
       (lambda (buf _mark)
	 (set-buffer buf)
	 (funcall mode))))))
(advice-add #'ibuffer-ext-do-change-major-mode :after #'ibuffer-ext--after-operation)

;;;###autoload
(defun ibuffer-ext-insert-independent-filter-group (name)
  "Insert the independent filter group associated with NAME.
NAME must be the string name of a filter group defined in
`user-ext-ibuffer-global-filter-groups', which see."
  (interactive (list (->> (mapcar #'car user-ext-ibuffer-global-filter-groups)
			  (completing-read "Filter Group: "))))
  (cl-check-type name string)
  (unless (eq major-mode 'ibuffer-mode)
    (user-error "Must be in IBuffer buffer"))
  (cl-symbol-macrolet ((fgroups user-ext-ibuffer-global-filter-groups))
    (when-let ((idx (--find-index (string= (car it) name) fgroups))
	       (group (nth idx fgroups)))
      
      (cl-pushnew group ibuffer-filter-groups)
      (emacs-version-cond-when-compile
	((> "27")
	 (ibuffer-filter-disable))
	(t (ibuffer-update nil t)))
      (message "Inserted independent filter group %S" name))))

;;;###autoload
(defun ibuffer-ext-toggle-current-filter-group ()
  "Expand/collapse the filter group of point.
If point is on a buffer, toggle the group it is in."
  (interactive)
  (unless (char-equal (char-after) ?\[)
    (ibuffer-backward-filter-group))
  (ibuffer-toggle-filter-group))


;; ### Hooks

;;;###autoload
(defun ibuffer--extra-hook () t)

;;;###autoload
(add-hook 'ibuffer-mode-hook #'ibuffer--extra-hook)


;; ### Keymaps

(define-key ibuffer-mode-map (kbd "/ T") #'ibuffer-ext-toggle-current-filter-group)
(define-key ibuffer-mode-map (kbd "/ G") #'ibuffer-ext-insert-independent-filter-group)
(define-key ibuffer-mode-map (kbd "C-x b") #'ibuffer-ext-buffer-name)
(define-key ibuffer-mode-map (kbd "M-M") #'ibuffer-ext-do-change-major-mode)


(extension-provide 'ibuffer-ext)
;;; ibuffer-ext.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "ix" "ibuffer-ext")
;; eval: (abbrev-ext-define-local-abbrev "ux" "user-ext-ibuffer")
;; End:
