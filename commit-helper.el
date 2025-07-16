;; -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'comint)
(require 'custom)
(require 'wid-browse)
(require 'wid-edit)
(require 's)
(require 'transient)

(eval-when-compile
  (require 'alist-ext)
  (require 'cl-ext)
  (require 'dash)
  (require 'subr-x)
  (require 'llama))

(load-extension "compat-29-ext")

;; ### Customization

(defgroup commit-helper-mode nil
  "Major mode for commit-helper dialog."
  :group 'user-extensions)

(defgroup commit-helper nil
  "Commit helper dialog."
  :group 'user-extensions)

(defcustom commit-helper-bin
  "~/.local/bin/commit"
  "Commit helper binary path."
  :type '(file :must-match t)
  :safe #'file-exists-p
  :group 'commit-helper)

(defcustom commit-helper-conventions
  (alist-ext-define "john" (list "add" "feature" "fix" "project"
				 "refactor" "remove" "update"))
  "A mapping of conventions to their respective tags."
  :group 'commit-helper
  :type '(alist :key-type string
		:value-type (repeat string))
  :safe #'commit-helper-tags-valid-p)

(defcustom commit-helper-convention "john"
  "The convention to use.
Must be a value from `commit-helper-conventions'."
  :group 'commit-helper
  :type 'string
  ;; :set #'commit-helper--set-convention
  :safe #'stringp)
;; (add-variable-watcher 'commit-helper-convention #'commit-helper--set-convention)

(defface commit-helper-button
  '((t (:inherit bold :background "light gray"
		 :box (:line-width 2 :color "dark gray" :style released-button))))
  "Face for buttons when they are not pressed."
  :group 'commit-helper)

(defface commit-helper-button-mouse
  '((t (:inherit bold :background "light gray"
		 :box (:line-width 2 :color "dark gray" :style released-button))))
  "Face for buttons when the mouse is above them."
  :group 'commit-helper)

(defface commit-helper-button-pressed
  '((t (:inherit bold :background "light gray"
		 :box (:line-width 2 :color "dark gray" :style pressed-button))))
  "Face for buttons when they are pressed."
  :group 'commit-helper)

;; ### Variables

;; (defvar commit-helper--convention-tags nil
;;   "Internal: List of tags of `commit-helper-convention'.")

(defconst commit-helper-buffer "*commit helper*"
  "The name of the buffer used by `commit-helper-dialog'.")

(defconst commit-helper--register ?c)

(defconst commit-helper-output-buffer "*commit helper help*"
  "The name of the output buffer.")

(defconst commit-helper-help-buffer "*commit helper output*"
  "The name of the helper buffer.")

(defvar commit-helper-no-interface nil
  "If non-nil, the buffer will not build the interface.")

(defvar commit-helper--convention-history nil "Internal.")

(defvar commit-helper--tag-history nil "Internal.")
(defvar commit-helper--context-history nil "Internal.")
(defvar commit-helper--message-history nil "Internal.")

(defvar-local commit-helper-global-data
  (list :dry-run nil))

(defvar-local commit-helper-working-directory nil
  "Working directory in `commit-helper-buffer'.")

;; ### Functions

(defsubst commit-helper-alist-get (key alist)
  "Find the element of ALIST whose key is KEY and return its `cdr'.
FInd the first element of ALIST whose `car' equals KEY and
return its `cdr'.

This function is inline: it expands to
   (alist-get KEY ALIST nil nil #\\='string=)"
  (declare (side-effect-free t))
  (alist-get key alist nil nil #'string=))

(defun commit-helper--find-git-root (&optional dir)
  "Return the root of the Git repository for DIR or `default-directory'.
Returns nil if not inside a Git repo."
  (let ((dir (file-name-as-directory (or dir default-directory))))
    (when-let ((root (locate-dominating-file dir ".git")))
      (file-name-as-directory (expand-file-name root)))))

(defun commit-helper--reset-history-vars ()
  "Internal: Reset history variables."
  (interactive)
  (setq commit-helper--context-history nil
	commit-helper--convention-history nil
	commit-helper--message-history nil
	commit-helper--tag-history nil))

(cl-defun commit-helper-get-files ()
  "Get the files that are recognized by Git."
  (with-temp-buffer
    (pcase (call-process "git" nil t nil "status" "--porcelain")
      ((and (pred stringp) signal)
       (message "Signal received: %s" signal)
       (cl-return-from commit-helper-get-files))
      (0 t)				; TODO: Error to buffer
      (code
       ;; And error was reported
       (cl-return-from commit-helper-get-files code)))
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^.. \\(.+\\)" nil t)
	     collect (cl-ext-progn
		       (match-string-no-properties 1)))))

(defun commit-helper-tags-valid-p (val)
  "Return t if VAL is valid for `commit-helper-tags', nil otherwise."
  (alist-of val 'string 'string))

(defun commit-helper--compile-command (tag message &optional context)
  "Return a command list for use by `commit-helper--run'."
  (cl-check-type tag string)
  (cl-check-type message string)
  (cl-check-type context (or string null))
  (let ((context (if (or (not context) (string-empty-p context))
		     "-" context)))
    (list commit-helper-bin "-t" tag "-m" message "-ct" context)))

(defsubst commit-helper--inside-buffer ()
  (and (eq major-mode 'commit-helper-mode)
       (equal (buffer-name) commit-helper-buffer)))

(defsubst commit-helper--error-if-outside-buffer ()
  (cl-ext-unless (commit-helper--inside-buffer)
      (user-error "Must be inside the commit buffer")))

(defmacro commit-helper-plist-put (plist prop value)
  "Set PLIST's PROP to VALUE."
  (cl-check-type plist symbol)
  `(setq ,plist (plist-put ,plist ,prop ,value)))

(defun commit-helper-format-commit (convention tag message &optional context)
  "Format a fake commit."
  ;; TODO: Finish docstring
  (cl-check-type convention string-or-null)
  (cl-check-type tag string)
  (cl-check-type message string)
  (cl-check-type context string-or-null)
  (-when-let (buffer (get-buffer "commit-temp"))
    (kill-buffer buffer))
  (let* ((process-connection-type nil)
	 (buffer-name "commit-temp")
	 (convention (or convention commit-helper-convention))
	 (process (start-process
		   "commit-temp" buffer-name
		   commit-helper-bin "-c" convention "-n")))
    (with-current-buffer buffer-name
      (process-send-string process (concat tag "\n"))
      (process-send-string process (concat message "\n"))
      (process-send-string process (or (concat context "\n") "\n"))
      (sleep-for 0.2)
      )))

(defun commit-helper-quit (&optional arg)
  "Quit the dialog buffer."
  (interactive "P")
  (commit-helper--error-if-outside-buffer)
  (if arg (kill-buffer) (quit-window))
  (jump-to-register commit-helper--register))

(defun commit-helper-no-edit ()
  "Warn the user the buffer can't be edited."
  (interactive)
  (user-error "Cannot edit this part of the buffer"))

(defun commit-helper--complete-convention (&optional convention tag msg context)
  "Complete the CONVENTION, TAG, MSG, and CONTEXT arguments.
Each argument controls whether its respective argument is
prompted: if, say, CONVENTION is non-nil, the user is
prompted for the convention; same for the other arguments."
  (let ((sconvention convention)
	result)
    (setq convention
	  (if convention
	      (completing-read
	       (format "Convention (default: %s): " commit-helper-convention)
	       (mapcar #'car commit-helper-conventions)
	       nil t nil 'commit-helper--convention-history
	       commit-helper-convention)
	    commit-helper-convention))
    (cl-ext-when sconvention
	(push convention result))
    (cl-ext-when tag
	(setq tag (completing-read
		   "Tag: "
		   (commit-helper-alist-get convention commit-helper-conventions)
		   nil nil nil 'commit-helper--tag-history))
      (push tag result))
    (cl-ext-when msg
	(setq msg (read-string "Message: " nil 'commit-helper--message-history))
      (push msg result))
    (cl-ext-when context
	(setq context (read-string "Context: " nil 'commit-helper--context-history "-"))
      (push context result))
    (nreverse result)))

(defsubst commit-helper--barf-if-not-in-prefix ()
  "Emit an error if caller is not called from within a prefix."
  (cl-ext-unless transient-current-prefix
      (error "Must be called from `commit-helper'")))

(defsubst commit-helper--barf-if-not-interactive (interactive-p)
  "Emit an error if caller is not called interactively."
  (cl-ext-unless interactive-p
      (error "Must be called as a command")))

;; --- Toplevel commands

(defun commit-helper-dialog (&optional arg)
  "Display a dialog to compose a commit."
  (interactive "P")
  (window-configuration-to-register commit-helper--register)
  (cl-ext-when (get-buffer commit-helper-buffer)
      (kill-buffer commit-helper-buffer))
  (commit-helper--kill-output-buffer)
  (let ((buffer (get-buffer-create commit-helper-buffer)))
    (if arg
	(pop-to-buffer buffer)
      (pop-to-buffer-same-window buffer))
    (commit-helper-mode)))

(defun commit-helper-change-convention (convention)
  "Change the current convention to CONVENTION."
  (interactive (list
		(completing-read (format "Convention (default: %s): "
					 commit-helper-convention)
				 (mapcar #'car commit-helper-conventions)
				 nil t nil nil commit-helper-convention)))
  (cl-ext-unless (cl-member convention (mapcar #'car commit-helper-conventions)
			    :test #'string=)
      (user-error "Invalid convention %s" convention))
  (setopt commit-helper-convention convention))

(defun commit-helper-customize-group ()
  "Display the customization group for this extension."
  (interactive)
  (customize-group 'commit-helper))

(defun commit-helper-commit (tag message &optional context dry-run interactive-p)
  "Format a commit with TAG, MESSAGE, and CONTEXT.
This command starts a subprocess with the name found in
`commit-helper-output-buffer'.  The arguments are sent to
the subprocess in order.

The buffer is this is called in's local value of
`default-directory' must be under a Git repository.

If CONTEXT is omitted or nil, an empty string is sent to the
subprocess when it prompts for the context.

When called interactively, TAG, MESSAGE, and CONTEXT are
prompted from the user, and DRY-RUN is the prefix argument."
  (interactive (cl-ext-progn
		 (commit-helper-barf--if-not-in-prefix)
		 (append (commit-helper--complete-convention nil t t t)
			 (transient-scope) t)))
  (commit-helper--barf-if-not-in-prefix)
  (commit-helper--barf-if-not-interactive)
  (cl-check-type tag string)
  (cl-check-type message string)
  (cl-check-type context (or string null))
  (window-configuration-to-register commit-helper--register)
  (commit-helper--kill-output-buffer)
  (if-let ((working-directory (commit-helper--find-git-root commit-helper-working-directory))
	   (delay 0.2)
	   (command (list commit-helper-bin))
	   (buffer-name commit-helper-output-buffer))
      (cl-ext-progn
	(cl-ext-when dry-run
	    (cl-ext-append-list "-n" command))
	(commit-helper--run-command command nil working-directory)
	(display-buffer buffer-name)
	(commit-helper--send-input tag nil delay)
	(commit-helper--send-input message nil delay)
	(commit-helper--send-input (pcase context
				     ((or 'nil "-" (pred string-empty-p))
				      "\n")
				     (_ context))
				   nil delay)
	(cl-ext-when dry-run
	    (while (comint-check-proc buffer-name)
	      (sleep-for 0.1))
	  (with-current-buffer buffer-name
	    (goto-char (point-min))
	    (if (search-forward-regexp "git commit -m [\"']\\(.+\\)[\"']" nil t)
		(cl-ext-progn
		  (kill-region (match-beginning 1) (match-end 1)))
	      (error "Failed to extract commit")))
	  (jump-to-register commit-helper--register)))
    (error "Not inside a Git repository")))

(transient-define-suffix commit-helper-commit-version (tag version &optional context dry-run)
  "Format a commit to mention a bump to VERSION.
TAG and CONTEXT are the same as for `commit-helper-commit'.
The message becomes \"Bump to version VERSION\".  If DRY-RUN
is non-nil, don't actually write the commit, but send it to
the kill ring instead.

When called interactively, The first three arguments are
prompted from the user, and DRY-RUN is the prefix argument."
  (interactive (-let (((tag context) (commit-helper--complete-convention nil t nil t))
		      (version (read-string "Version: " nil t)))
		 (list tag version context (transient-scope))))
  (commit-helper-commit tag (format "Bump to version %s" version) context dry-run))

(defun commit-helper-convention-help (convention)
  "Show the help message for CONVENTION."
  (interactive (commit-helper--complete-convention t))
  (let (doc)
    (with-temp-buffer
      (pcase (call-process-shell-command (format "commit -sc %s" convention)
					 nil t)
	((and (pred stringp) err)
	 (error "Signal returned from process: %s" err))
	(0 (setq doc (buffer-string)))
	(code (error "Exit status returned: %d" code))))
    (with-help-window (get-buffer-create commit-helper-help-buffer)
      (princ doc))))

;;;###autoload (autoload 'commit-helper "commit-helper" nil t)
(transient-define-prefix commit-helper (prefix)
  "Entry point to commit helper commands."
  ["Commit Helper commands"
   ("c" "Format a commit" commit-helper-commit)
   ("d" "Show dialog" commit-helper-dialog)
   ("v" "Format a version commit" commit-helper-commit-version)
   " "
   ("C" "Change convention" commit-helper-change-convention :transient t)
   ("h" "Convention help" commit-helper-convention-help)
   ("g" "Customize group" commit-helper-customize-group)]
  (interactive "P")
  (transient-setup 'commit-helper nil nil :scope prefix))

;; --- Process output buffer

(defun commit-helper--send-input (input &optional eof delay)
  "Send INPUT to the commit-helper process in the output buffer.
If EOF is non-nil, send an EOF after input.
If DELAY is specified (in seconds), wait before sending."
  (let ((buffer (get-buffer commit-helper-output-buffer)))
    (if (not (buffer-live-p buffer))
        (message "No active commit-helper buffer.")
      (with-current-buffer buffer
        (if-let ((proc (get-buffer-process buffer)))
	    (cl-ext-progn
	      (cl-ext-when delay
		  (sit-for delay))
              (accept-process-output proc 0.2) ; Give process time to prompt
	      (insert input)
	      (comint-send-input)
              (cl-ext-when eof
		  (comint-send-eof)))
          (error "No running process in buffer."))))))

(defun commit-helper-process--filter-output (_string)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region comint-last-output-start (point))))

(defun commit-helper-process--filter-preoutput (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (not (eobp))
      (fixup-whitespace)
      (forward-line 1))
    (setq string (buffer-string)))
  string)

(defconst commit-helper-process-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode commit-helper-process-mode comint-mode "Commit-Helper"
  "Major mode for the `commit' program."
  (setq-local comint-process-echoes nil)
  (add-hook 'comint-output-filter-functions
	    #'commit-helper-process--filter-output nil t)
  (add-hook 'comint-preoutput-filter-functions
	    #'commit-helper-process--filter-preoutput nil t))

(defsubst commit-helper--kill-output-buffer ()
  (cl-ext-when (get-buffer commit-helper-output-buffer)
      (kill-buffer commit-helper-output-buffer)
    t))

(defun commit-helper--run-command (command &optional dry-run working-directory)
  "Run COMMAND and pipe output to a buffer."
  (commit-helper--kill-output-buffer)
  (let ((buffer (get-buffer-create commit-helper-output-buffer))
	(working-directory (or working-directory default-directory)))
    (with-current-buffer buffer
      (commit-helper-process-mode)
      (setq default-directory (file-name-as-directory working-directory)
	    header-line-format (format "Working directory: %s" default-directory))
      (message "Working directory set to %s" default-directory))
    (-let* (((program . args) command)
	    (program-name (f-filename program)))
      (cl-assert (file-directory-p working-directory) t)
      (if dry-run
	  (cl-ext-progn
	    (--print-expr var command))
	(comint-exec buffer program-name program nil args)))))

(defun commit-helper--create-interface ()
  (unwind-protect
      (let* ((tags (alist-get commit-helper-convention commit-helper-conventions
			      nil nil #'equal))
	     (tag-items (cl-loop for tag in tags
			     collect
			     `(item :tag ,tag ,tag)))
	     (button-quit (##commit-helper-quit t _%1 _%2)))
	(cl-ext-unless tags
	    (error "Invalid convention %s" commit-helper-convention))
	(let ((inhibit-read-only t))
	    (erase-buffer))
	(remove-overlays)
	(let (make-data wtag wmsg wct)
	  (setq make-data (lambda ()
			    (list :message (widget-value wmsg)
				  :tag (widget-value wtag)
				  :context (widget-value wct))))
	  (widget-insert (format "Working directory: %s\nUsing convention %s.\n\n"
				 default-directory
				 (propertize commit-helper-convention
					     'face 'bold)))
	  (widget-create
	   'commit-helper-vbox
	   :tag "Options"
	   `(commit-helper-checkbox
	     :tag "Dry run"
	     :notify ,(lambda (widget &rest _ignore)
			(commit-helper-plist-put commit-helper-global-data
						 :dry-run
						 (widget-value widget)))
	     (plist-get commit-helper-global-data :dry-run)))
	  (widget-insert "\n\n")
	  (setq wtag (apply #'widget-create ; Dropdown menu with tag options
			    'menu-choice
			    :tag "Tag"
			    :value (car tags)
			    tag-items)
		wmsg (widget-create 'commit-helper-editable-field
				    :tag "Message")
		wct (widget-create 'commit-helper-editable-field
				   :tag "Context"))
	  (widget-create 'commit-helper-button
			 :action (##commit-helper--submit-commit
				  (funcall make-data) %1 %2)
			 "Submit")
	  (widget-create 'commit-helper-button
			 :action button-quit
			 "Cancel")
	  (widget-insert "\n\n"))
	(let* ((files (commit-helper-get-files))
	       (file-completion (completion-table-dynamic
				 (lambda (prefix)
				   (--filter (s-prefix-p prefix it) files))))
	       wvbox make-data)
	  ;; Add/update/remove files
	  (setq make-data (lambda ()
			    ;; wvbox's children are, in order, the tag, file, and action;
			    ;; choice, file, and radio button, respectively
			    (cl-destructuring-bind (wtag wfile waction &rest ignore)
				(widget-get wvbox :children)
			      (cl-ext-when (widget-apply wfile :validate)
				  (user-error (widget-get wfile :error)))
			      (list :tag (widget-value wtag)
				    :file (widget-value wfile)
				    :action (widget-value waction))))
		wvbox
		(widget-create
		 'commit-helper-vbox
		 :tag "Add/Update/Remove Files"
		 `(menu-choice
		   :tag "Tag"
		   :value ,(car tags)
		   ,@tag-items)
		 ;; File entry
		 (list 'commit-helper-file
		       :completions file-completion)
		 ;; Radio: action to take with file
		 (list 'commit-helper-radio-choice
		       :value "none"
		       :tag "Auto Git Action"
		       '(item "add")
		       '(item "remove")
		       '(item "none"))
		 ;; Submit button
		 (list 'commit-helper-button
		       :action (lambda (widget &optional event)
				 (commit-helper--submit-file-update
				  (funcall make-data) widget event))
		       "Submit")
		 ;; Cancel button
		 (list 'commit-helper-button
		       :action button-quit
		       "Cancel"))))
	(widget-setup))
    (goto-char (point-min))))

(defconst commit-helper-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (cl-loop for key being the key-seqs of widget-keymap
	     using (key-bindings def)
	     do
	     (define-key map key def))
    (define-key map [remap self-insert-command] #'commit-helper-no-edit)
    (define-key map [remap quit-window] #'commit-helper-quit)
    (define-key map (kbd "C-M-i") #'widget-complete)
    map))

(defsubst commit-helper-reset-vars ()
  (setq commit-helper-global-data (list :dry-run nil)
	commit-helper-working-directory nil))

(define-derived-mode commit-helper-mode special-mode
  "Commit Helper"
  "Major mode for composing commits with commit helper."
  (setq-local widget-push-button-prefix ""
	      widget-push-button-suffix ""
	      widget-button-face 'commit-helper-button
	      widget-button-pressed-face 'commit-helper-button-pressed
	      widget-mouse-face 'commit-helper-button-mouse)
  (cl-ext-unless commit-helper-no-interface
      (commit-helper--create-interface))
  (commit-helper-reset-vars)
  (setq commit-helper-no-interface nil
	commit-helper-working-directory default-directory)
  (read-only-mode 0))

;; --- Callbacks

(defun commit-helper--submit-commit (data _widget &optional _event)
  (-let ((wd commit-helper-working-directory)
	 ((&plist :tag tag :message message :context context) data)
	 (dry-run (plist-get commit-helper-global-data :dry-run)))
    (cl-assert wd)
    (commit-helper-quit t)
    (commit-helper-commit tag message context dry-run)))

(defun commit-helper--submit-file-update (data _widget &optional _event)
  (commit-helper-quit t)
  (-let (((&plist :file file :action action) data)
	 ((&plist :dry-run dry-run)))
    (let ((commit-helper-output-buffer "*git*"))
      (pcase action
	("add"
	 (commit-helper--run-command
	  (list "git" "add" file)
	  dry-run
	  (commit-helper--find-git-root commit-helper-working-directory)))
	("remove"
	 (commit-helper--run-command
	  (if (f-exists-p file)
	      (list "git" "rm" file)
	    (list "git" "rm" "--cached" file))))))
    (if (plist-get commit-helper-global-data :dry-run)
	(cl-ext-progn
	  t)
      (-let (((&plist :tag tag :file file :context context) data))
	(commit-helper-commit tag file context)))))

;; --- Widgets

(define-widget 'commit-helper-checkbox 'checkbox
  "A checkbox for `commit-helper-dialog'."
  :format "%[%v%] %t")

(define-widget 'commit-helper-button 'push-button
  "A button for `commit-helper-dialog'."
  :format "%[%v%] ")

(defconst commit-helper-editable-field-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-k") #'widget-kill-line)
    (define-key map (kbd "C-M-i") #'widget-complete)
    (define-key map (kbd "<return>") #'widget-field-activate)
    ;; Since the widget code uses a `field' property to identify fields,
    ;; ordinary beginning-of-line does the right thing.
    ;;  (define-key map "\C-a" 'widget-beginning-of-line)
    (define-key map (kbd "<end>") #'widget-end-of-line)
    map))

(defconst commit-helper-file-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map commit-helper-editable-field-keymap)
    map))

(define-widget 'commit-helper-editable-field 'editable-field
  "An editable text field for `commit-helper-dialog'."
  :format "%{%t%}: %v"
  :keymap commit-helper-file-keymap
  :help-echo "C-M-i: complete field; RET: enter value")

(define-widget 'commit-helper-vbox 'group
  "A vertical box of widgets.

Syntax:
  TYPE ::= (commit-helper-vbox [KEYWORD ARGUMENT]... TYPE...)

Items are arranged vertically starting from the top.  The
:tag sets the label under which the items are arranged.

The value is a list, with one member for each TYPE.  That is
to say, each TYPE is a list starting with the name of a
widget, followed by the arguments used to construct it."
  :indent 2
  :format "%{%t%}:\n%v")

(define-widget 'commit-helper-checkbox 'checkbox
  "A checkbox for `commit-helper-dialog'.
Use :tag to add a label.  The :value, which can be specified
at the end of the argument list, is a boolean flag.

TYPE = \"commit-helper-checkbox \" , { KEYWORD , \" \" , ARGUMENT ] } , [ VALUE ] .

Here is an example of to use this:
   (widget-create
    \\='commit-helper-checkbox
    :tag \"Label\"
    :notify (lambda (widget &rest ignore)
               (widget-value widget))
    nil)"
  :format "%[%v%] %t")

(define-widget 'commit-helper-radio-choice 'radio-button-choice
  "Select one of mutiple options"
  :format "%t\n%v")

(defun commit-helper-file-action (widget &optional event)
  (let (;; (pos (point))
	(file (read-file-name "File: ")))
    (widget-value-set widget (file-relative-name file))
    (widget-default-action widget event)))

(define-widget 'commit-helper-file 'file
  "A file widget for `commit-helper-dialog'."
  :format "%{%t%}: %v"
  :keymap commit-helper-editable-field-keymap
  :error "Missing file"
  :validate (lambda (widget)
	      (cl-ext-when (string-empty-p (widget-value widget))
		  widget))
  :help-echo "C-M-i: complete field; RET: enter value")

(provide 'commit-helper)
;;; commit-helper.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "comh" "commit-helper")
;; End:
