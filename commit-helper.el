;; -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'comint)
(require 'custom)
(require 'wid-browse)
(require 'wid-edit)
(require 's)
(require 'transient)

(eval-when-compile
  (require 'debug-ext)
  (require 'alist-ext)
  (require 'cl-ext)
  (require 'dash)
  (require 'debug-ext)
  (require 'subr-x)
  (require 'llama))

(load-extension "compat-29-ext")

(cl-defstruct (commit-helper-convention
	       (:type list)
	       :named)
  "Represents a convention."
  (name nil :type string)
  (tags nil :type list)
  (tag-transform nil :type function)
  (format nil :type string)
  (format-no-context nil :type string)
  (context-transform nil :type function))

(cl-defstruct (commit-helper-commit
	       (:type list))
  "Represents a commit."
  context message tag dry-run)

(defsubst commit-helper-convention-has-context-p (cl-x)
  "Return non-nil if CL-X supports context."
  (let ((fmt (commit-helper-convention-format cl-x)))
    (or (null fmt) (string-empty-p fmt))))


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
(make-obsolete-variable 'commit-helper-bin 'commit-helper-git-bin "2025-12-10")

(defcustom commit-helper-git-bin
  "/usr/bin/git"
  "Path to the Git binary."
  :type '(file :must-match t)
  :safe #'file-exists-p
  :group #'commit-helper)

(defcustom commit-helper-conventions
  (alist-ext-define
   "john" (make-commit-helper-convention
	   :name "john"
	   :tags (list "add" "feature" "fix" "project" "refactor" "remove" "update")
	   :tag-transform #'upcase
	   :format "[%s] %s: %s"
	   :format-no-context "[%s] %s"
	   :context-transform #'capitalize))
  "A mapping of convention names and their specifications."
  :group 'commit-helper
  :type '(alist
	  :key-type string
	  :value-type (list
		       (const commit-helper-convention)
		       (string :tag "Name")
		       (repeat (string :tag "Tag"))
		       (symbol :tag "Tag Transform Function")
		       (string :tag "Format")
		       (string :tag "Format No Context")
		       (symbol :tag "Context Transform Function")))
  :risky t)
(--ignore
 (--destroy-variable 'commit-helper-conventions)
 t)

(defcustom commit-helper-default-convention "john"
  "The default convention to use."
  :group 'commit-helper
  :type 'string
  :safe #'stringp)

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

(defconst commit-helper-buffer "*commit helper*"
  "The name of the buffer used by `commit-helper-dialog'.")

(defconst commit-helper--register ?c)

(defconst commit-helper-output-buffer "*commit helper help*"
  "The name of the output buffer.")

(defconst commit-helper-help-buffer "*commit helper output*"
  "The name of the helper buffer.")

(defvar commit-helper-no-interface nil
  "If non-nil, the buffer will not build the interface.")

(defvar commit-helper-convention "john"
  "The current convention.")

;; History vars
(defvar commit-helper--convention-history nil "Internal.")
(defvar commit-helper--tag-history nil "Internal.")
(defvar commit-helper--context-history nil "Internal.")
(defvar commit-helper--message-history nil "Internal.")
(defvar commit-helper--dependency-history nil "Internal.")

(defvar commit-helper-commit-history nil
  "History of previous commits.
Each item is of type `commit-helper-commit'.")

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
	commit-helper--tag-history nil
	commit-helper-commit-history nil))

(cl-defun commit-helper-get-files ()
  "Get the files that are recognized by Git."
  (with-temp-buffer
    (pcase (call-process "git" nil t nil "status" "--porcelain")
      ((and (pred stringp) signal)
       (message "Signal received: %s" signal)
       (cl-return-from commit-helper-get-files))
      (0 t)				; TODO: Error to buffer
      (code
       ;; An error was reported
       (cl-return-from commit-helper-get-files code)))
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^.. \\(.+\\)" nil t)
	     collect (cl-ext-progn
		       (match-string-no-properties 1)))))

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

(defmacro commit-helper-plist-put (plist prop value)
  "Set PLIST's PROP to VALUE."
  (cl-check-type plist symbol)
  `(setq ,plist (plist-put ,plist ,prop ,value)))

(defun commit-helper-format-commit
    (convention tag message &optional context wip)
  "Format a commit for CONVENTION.
The commit consists of TAG, MESSAGE and CONTEXT."
  (if-let ((conv (commit-helper-alist-get
		  convention commit-helper-conventions)))
      (let ((tag (if-let ((fun (commit-helper-convention-tag-transform conv)))
		     (funcall fun tag)
		   tag))
	    (fmt (or (and context
			  (not (string= context "-"))
			  (commit-helper-convention-format conv))
		     (commit-helper-convention-format-no-context conv))))
	(concat (if wip "WIP: " "")
		(apply
		 #'format fmt
		 (or (and context
			  (not (string= context "-"))
			  (list tag context message))
		     (list tag message)))))
    (error "There is no %s convention" convention)))

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


;; --- Completion functions

(defun commit-helper--complete-convention (&optional convention tag msg context)
  "Complete the CONVENTION, TAG, MSG, and CONTEXT arguments.
Each argument controls whether its respective argument is
prompted: if, say, CONVENTION is non-nil, the user is
prompted for the convention; same for the other arguments.

The result is a list comprised of the completion results for
the non-nil arguments (e.g., a tag if TAG is non-nil).  If
only one argument is non-nil, the result will in turn not be
a list but the corresponding result (e.g., a string if only
TAG is non-nil).

Example: 

   (commit-helper--complete-convention nil t t)
   =>
   (TAG MESSAGE)"
  (let ((sconvention convention)
	result)
    (setq convention
	  (if convention
	      (or (--> (completing-read
			(format "Convention (default: %s): " commit-helper-convention)
			(cl-loop for (k . _v) in commit-helper-conventions
				 collect k)
			nil t nil 'commit-helper--convention-history
			commit-helper-convention)
		       (commit-helper-alist-get it commit-helper-conventions))
		  (error "There is no %s convention" convention))
	    (or (commit-helper-alist-get
		 commit-helper-convention commit-helper-conventions)
		(error "There is no %s convention" commit-helper-convention))))
    (and sconvention (push convention result))
    (when tag
      (setq tag (completing-read
		 "Tag: "
		 (commit-helper-convention-tags convention)
		 nil nil nil 'commit-helper--tag-history))
      (push tag result))
    (when msg
      (setq msg (read-string "Message: " nil 'commit-helper--message-history))
      (push msg result))
    (when context
      (setq context (read-string "Context: " nil 'commit-helper--context-history "-"))
      (push context result))
    (setq result (nreverse result))
    (if (= (length result) 1)
	(car result) result)))
(--ignore
 (commit-helper--complete-convention nil t t)
 t)

(defun commit-helper--complete-commit ()
  (or commit-helper-commit-history
      (user-error "Empty history"))
  (let* ((hist (make-hash-table :test #'equal))
	 (temp (completing-read
		"Commit: "
		(mapcar
		 (lambda (commit)
		   (let ((key (commit-helper-format-commit
			       commit-helper-convention
			       (--print-expr sexp (commit-helper-commit-tag commit))
			       (commit-helper-commit-message commit)
			       (commit-helper-commit-context commit))))
		     (prog1 key
		       (puthash key commit hist))))
		 commit-helper-commit-history))))
    (or (gethash temp hist)
	(error "No commit found"))))
(--ignore
 (setq temp (make-commit-helper-commit
	     :context "debug"
	     :tag "fix"
	     :message "Incorrect info file path"))

 (add-to-history
  'commit-helper-commit-history
  temp)
 t)


;; --- Errors

(defsubst commit-helper--error-if-outside-buffer ()
  (unless (commit-helper--inside-buffer)
    (user-error "Must be inside the commit buffer")))

(defsubst commit-helper--barf-if-not-in-prefix ()
  "Emit an error if caller is not called from within a prefix."
  (unless transient-current-prefix
    (error "Must be called from `commit-helper'")))

(defsubst commit-helper--barf-if-not-interactive (interactive-p)
  "Emit an error if caller is not called interactively."
  (or interactive-p
      (error "Must be called as a command")))


;; --- Toplevel commands

(defun commit-helper-dialog (&optional arg)
  "Display a dialog to compose a commit."
  (interactive "P")
  (window-configuration-to-register commit-helper--register)
  (when (get-buffer commit-helper-buffer)
    (kill-buffer commit-helper-buffer))
  (commit-helper--kill-output-buffer)
  (let ((buffer (get-buffer-create commit-helper-buffer)))
    (if arg
	(pop-to-buffer buffer)
      (pop-to-buffer-same-window buffer))
    (commit-helper-mode)))

(defun commit-helper-change-convention (convention)
  "Change the current convention to CONVENTION.
CONVENTION can either a string or of type `commit-helper-convention'."
  (interactive (commit-helper--complete-convention t))
  (cl-check-type convention (or string commit-helper-convention))
  (setq commit-helper-convention (if (stringp convention)
				     convention
				   (commit-helper-convention-name convention))))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'commit-helper-change-convention))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t)
   (call-interactively #'menu-bar--toggle-truncate-long-lines))
 t)

(defun commit-helper-customize-group ()
  "Display the customization group for this extension."
  (interactive)
  (customize-group 'commit-helper))

(transient-define-suffix commit-helper-commit-repeat (commit)
  (interactive (commit-helper--complete-commit))
  (cl-check-type commit list)
  (commit-helper-commit
   (commit-helper-commit-tag commit)
   (commit-helper-commit-message commit)
   (commit-helper-commit-context commit)))

(transient-define-suffix commit-helper-commit
  (tag message &optional context dry-run wip)
  "Format a commit with TAG, MESSAGE, and CONTEXT.
This command starts a subprocess with the name found in
`commit-helper-output-buffer'.  The arguments are sent to
the subprocess in order.

The buffer is this is called in's local value of
`default-directory' must be under a Git repository.

If CONTEXT is omitted or nil, an empty string is sent to the
subprocess when it prompts for the context.

When called interactively, TAG, MESSAGE, and CONTEXT are
prompted from the user, and DRY-RUN is the prefix argument.

\(fn TAG MESSAGE &optional CONTEXT DRY-RUN)"
  (interactive (append (commit-helper--complete-convention nil t t t)
		       (list (transient-scope transient-current-command)
			     (transient-arg-value
			      "--wip" (transient-args transient-current-command)))))
  (cl-check-type tag string)
  (cl-check-type message string)
  (cl-check-type context (or string null))
  (window-configuration-to-register commit-helper--register)
  (commit-helper--kill-output-buffer)
  (if-let ((working-directory (commit-helper--find-git-root commit-helper-working-directory)))
      (let ((msg (commit-helper-format-commit
		  commit-helper-convention tag message context wip)))
	(if dry-run
	    (with-temp-buffer
	      (insert msg)
	      (kill-region (point-min) (point-max)))
	  (let ((command (list commit-helper-git-bin "commit" "-m" msg)))
	    (commit-helper--run-command command dry-run working-directory)
	    (pop-to-buffer commit-helper-output-buffer)
	    (add-to-history
	     'commit-helper-commit-history
	     (make-commit-helper-commit
	      :tag tag :message message :context context)))))
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

(transient-define-suffix commit-helper-dependency (tag action dep &optional group context dry-run)
  "Format a commit about a dependency."
  (interactive (-let (((tag context) (commit-helper--complete-convention nil t nil t))
		      (args (transient-args transient-current-command)))
		 (list tag
		       (transient-arg-value "--action=" args)
		       (read-string "Dependency: " nil 'commit-helper--dependency-history)
		       (if (transient-arg-value "--dev" args)
			   "dev" (transient-arg-value "--group=" args))
		       context
		       (transient-scope transient-current-command))))
  (cl-check-type tag string)
  (cl-check-type action string)
  (cl-check-type dep string)
  (cl-check-type group string-or-null)
  (cl-check-type context string-or-null)
  ;; Commit "[TAG] ACTION DEPENDENCY (GROUP)"
  ;; Commit "[TAG] ACTION DEPENDENCY"
  (let ((msg (if group (format "%s %s (%s)" action dep group)
	       (format "%s %s" action dep))))
    (commit-helper-commit tag msg context dry-run)))

(defun commit-helper-convention-help (convention)
  "Show the help message for CONVENTION."
  (interactive (list (commit-helper--complete-convention t)))
  (let ((convention (commit-helper-convention-name convention))
	doc)
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
   ("-w" "WIP commit" "--wip")
   " "
   ("c" "Format a commit" commit-helper-commit)
   ("r" "Repeat the a previous commit" commit-helper-commit-repeat
    :if (##not (null commit-helper-commit-history)))
   ("d" "Show dialog" commit-helper-dialog)
   ("v" "Format a version commit" commit-helper-commit-version)]
  ["Add dependencies"
   ("-d" "dev group" "--dev")
   ("-g" "group" "--group=")
   ("-a" "action" "--action="
    :choices ("Add" "Remove" "Upgrade")
    :init-value (##oset %1 value "Add")
    :always-read t)
   " "
   ("D" "Format a dependency commit" commit-helper-dependency)]
  ["Misc"
   ("C" "Change convention" commit-helper-change-convention :transient t)
   ("h" "Convention help" commit-helper-convention-help)
   ("g" "Customize group" commit-helper-customize-group)
   ("q" "Quit" keyboard-quit)]
  (interactive "P")
  (transient-setup 'commit-helper nil nil :scope prefix))
(--ignore
 (prog1 nil
   (with-current-buffer (get-buffer-create "*output*")
     (emacs-lisp-mode)
     (cl-prettyprint (symbol-function #'commit-helper))
     (newline)
     (--symbol-plist #'commit-helper)
     (newline)
     (cl-prettyprint (symbol-function #'transient:commit-helper:--action=))
     (newline)
     (cl-prettyprint (symbol-function #'transient--default-infix-command))
     (run-with-idle-timer 0.2 nil #'activate-view-mode 1)
     (set-buffer-modified-p nil))
   (pop-to-buffer "*output*" t)
   (call-interactively #'menu-bar--toggle-truncate-long-lines))
 t)


;; --- Process output buffer

(defun commit-helper--send-input (input &optional eof delay)
  "Send INPUT to the commit-helper process in the output buffer.
If EOF is non-nil, send an EOF after input.
If DELAY is specified (in seconds), wait before sending."
  (let ((buffer (get-buffer commit-helper-output-buffer)))
    (if (not (buffer-live-p buffer))
        (message "No active commit-helper buffer.")
      (with-current-buffer buffer
        (condition-case err
	    (if-let ((proc (get-buffer-process buffer)))
		(cl-ext-progn
		  (when delay
		    (sit-for delay))
		  (accept-process-output proc 0.5)
		  (insert input)
		  (comint-send-input)
		  (when eof
		    (comint-send-eof)))
              (error "No running process in buffer."))
	  (error (message "Error from `commit-helper--send-input': %S" err)
		 (ignore-errors (comint-send-eof))))))))
(--ignore
 (cl-prettyprint (symbol-function 'commit-helper--send-input))
 t)

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
  (when (get-buffer commit-helper-output-buffer)
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


;; ### Commit Helper Mode

(defun commit-helper--create-interface ()
  (unwind-protect
      (let* ((conv (or (commit-helper-alist-get commit-helper-convention commit-helper-conventions)
		       (error "Unknown convention: %s" commit-helper-convention)))
	     (tags (commit-helper-convention-tags conv))
	     (tag-items (cl-loop for tag in tags
				 collect
				 `(item :tag ,tag ,tag)))
	     (button-quit (##commit-helper-quit t _%1 _%2)))
	(or tags
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
			      (--print-expr sexp (widget-value wfile))
			      (or (widget-apply wfile :validate)
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
  (unless commit-helper-no-interface
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


;; ### Widgets

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
	      (unless (string-empty-p (widget-value widget))
		widget))
  :help-echo "C-M-i: complete field; RET: enter value")

(provide 'commit-helper)
;;; commit-helper.el ends here

;; Local Variables:
;; eval: (abbrev-ext-install-local-abbrev-functions)
;; eval: (abbrev-ext-define-local-abbrev "comh" "commit-helper")
;; eval: (abbrev-ext-define-local-abbrev "comhc" "commit-helper-commit")
;; eval: (abbrev-ext-define-local-abbrev "comhv" "commit-helper-convention")
;; End:
