;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'custom)

(eval-and-compile
  (embed-doc-document-symbol buffers-ext
    "Buffer-related functions."
    :commands
    activate-view-mode
    clone-and-view-buffer
    clone-indirect-buffer-this-window
    faces-buffer
    general-scratch
    kill-buffers
    kill-certain-temp-buffers
    kill-customization-buffers
    revert-all-buffers
    set-current-window-dedicated
    tmpbuf
    :functions
    define-scratch-buffer-function
    with-tmpbuf
    :customs
    user-ext-temp-buffers-to-kill
    user-ext-temp-buffers-to-kill-regex))

;; ### Customization

(defgroup buffers-ext nil
  "Group for custom buffer-related options."
  :group 'user-extensions)

;; TODO: Add a setter that compiles this list into a regular expression so the functions can use that
(defcustom user-ext-temp-buffers-to-kill
  '("*Backtrace*" "*comment-tags*"
    "*Buffer List*"
    "*Completions*"
    "*Compile-Log*"
    "*Occur*"
    "*Packages*"
    "*Shell Command Output*")
  "A list of buffers to kill when `kill-certain-temp-buffers' is called."
  :type '(repeat string)
  :safe 'listp
  :group 'buffers-ext)

(defcustom user-ext-temp-buffers-to-kill-regex
  '("\\*.*[hH]elp.*" "\\*vc-.*")
  "A list of regexes of buffers to kill when `kill-certain-temp-buffers' is called."
  :type '(repeat regexp)
  :safe 'listp
  :group 'buffers-ext)

;; ### Functions

(defun set-current-window-dedicated (window flag)
  "Set the dedicated status of the WINDOW to FLAG.
The exact behavior depends on FLAG:
- If FLAG is 1 or the symbol `toggle', flip the window's
dedicated status between on and off.
- If it is 4 or t, then set the window's dedicated status.
- If it is 16 or nil, then clear the window's dedicated
status.

When called interfactively, FLAG is the numeric value of the
prefix argument:
- No prefix arg to toggle the window's status.
- \\[universal-argument] to set it.
- \\[universal-argument] \\[universal-argument] to clear it."
  (interactive (list (selected-window)
		     (prefix-numeric-value current-prefix-arg)))
  (let ((msg (pcase flag
	       ((or 1 'toggle)
		(if (window-dedicated-p window)
		    "Released window from being dedicated"
		  "Set window to be dedicated"))
	       ((or 4 't)
		"Set window to be dedicated")
	       ((or 16 'nil)
		"Released window from being dedicated"))))
    (set-window-dedicated-p
     window
     (pcase flag
       ((or 1 'toggle) (not (window-dedicated-p window)))
       ((or 4 't) t)
       ((or 16 'nil) nil)))
    (message msg)))

(defmacro with-tmpbuf (buffer-name &rest body)
  "Create a temporary buffer and evaluate BODY like `progn'.
The buffer's name is BUFFER-NAME.

The buffer is killed at the end once all BODY forms are
evaluated."
  (declare (indent 1) (debug t))
  (cl-check-type buffer-name string)
  `(let ((buffer (tmpbuf ,buffer-name)))
     (unwind-protect
	 (progn
	   (switch-to-buffer buffer)
	   ,@body)
       (kill-buffer buffer))))

(provide 'buffers-ext)

;;; buffers-ext ends here
