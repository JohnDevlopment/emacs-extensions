(eval-when-compile
  (require 'dired))

;; Customization

(defgroup buffers-ext nil
  "Group for custom buffer-related options."
  :group 'user-extensions)

(defcustom user-ext-temp-buffers-to-kill
  '(
    "*Backtrace*" "*comment-tags*"
    "*Buffer List*"
    "*Completions*"
    "*Compile-Log*"
    "*Occur*"
    "*Packages*"
    "*Shell Command Output*"
    )
  "A list of buffers to kill when `kill-certain-temp-buffers' is called."
  :type '(repeat string)
  :safe 'list-p
  :group 'buffers-ext)

(defcustom user-ext-temp-buffers-to-kill-regex
  '(
    "\\*.*[hH]elp.*"
    "\\*vc-.*"
    )
  "A list of regexes of buffers to kill when `kill-certain-temp-buffers' is called."
  :type '(repeat regexp)
  :safe 'list-p
  :group 'buffers-ext)

;; Functions to kill buffers matching patterns

(defun kill-certain-temp-buffers ()
  "Convenience function to kill certain buffers you do not need.

This kills buffers belonging to `user-ext-temp-buffers-to-kill' and
`user-ext-temp-buffers-to-kill-regex'."
  (interactive)
  (let ((bl user-ext-temp-buffers-to-kill)
	(rl user-ext-temp-buffers-to-kill-regex)
	bn buf)
    ;; Kill buffers in `user-ext-temp-buffers-to-kill'
    (dolist (bn bl)
      (setq buf (get-buffer bn))
      (when buf
	(kill-buffer buf)
	(message "killed %s" bn)))
    (dolist (regexp rl)
      (kill-buffers regexp))))

(defun kill-lsp-buffers ()
  "Kill all buffers that have to do with function `lsp-mode'."
  (interactive)
  (kill-buffers "\*.*ls.*")
  (kill-buffers "\*pyright.*")
  (kill-buffer "*Flymake log*"))

(defun kill-customization-buffers ()
  "Close all customization buffers.
Internally, calls `kill-buffers' with \"^\*Customize.*\" as the pattern."
  (interactive)
  (kill-buffers "\\*Customize.*"))

(defun kill-flymake-diagnostics ()
  "Close all buffers for flymake diagnostics.
Internally, calls `kill-buffers' with the pattern
\"^\*Flymake diagnostics for .*\""
  (interactive)
  (kill-buffers "\\*Flymake diag.*"))

(defun kill-buffers (pattern)
  "Close all buffers matching PATTERN."
  (interactive "sRegexp: ")
  (let ((bl (buffer-list))
	idx)
    (dolist (buf bl)
      (setq idx (string-match-p pattern
				(buffer-name buf)))
      (when (integerp idx)
	    (message "Killed %s" (buffer-name buf))
	    (kill-buffer (buffer-name buf))))))

;; ---

(defun get-buffer-file-name ()
  "Print the file belonging to the current buffer."
  (interactive)
  (let ((pos (point-marker)))
    (insert (buffer-file-name))
    (kill-region pos (point))))

;; Functions that create scratch or temp buffers

(defun faces-buffer ()
  "Open a buffer listing all the faces."
  (interactive)
  (let (faces face)
    (setq faces (seq-map 'symbol-name (face-list)))
    (tmpbuf "faces")
    (dolist (face (seq-sort 'string< faces))
      (insert face)
      (newline))))

(defun docstring-scratch (buffer-name &optional fill-number)
  "Open a scratch buffer for documentation strings.

Creates a temporary buffer with the name BUFFER-NAME.  The
newly created buffer has `auto-fill-mode' enabled.  Its
`fill-column' is set to FILL-NUMBER (if non-nil) or 60
otherwise.

When called interactively, FILL-NUMBER is the prefix arg."
  (interactive "sBuffer (default: \"docstring\"): \nP")
  (let* ((bufname (if (> (length buffer-name) 0)
		      buffer-name
		    "docstring"))
	 (buffer (tmpbuf bufname))
	 (fill-number (or fill-number 60)))
    (switch-to-buffer buffer)
    (auto-fill-mode t)
    (when fill-number
      (set-fill-column fill-number))))

(defun git-commit-scratch ()
  "Open a scratch buffer to let you format a git commit."
  (interactive)
  (tmpbuf "git commit" t)
  (auto-fill-mode t)
  (set-fill-column 50))

(defun python-scratch ()
  "Open a scratch buffer for Python code."
  (interactive)
  (tmpbuf "python" t)
  (python-mode)
  (electric-pair-local-mode 1))

(defun tmpbuf (buf &optional switch)
  "Open a temporary buffer.

If it doesn't exist, open a new one.  BUF is the name of the
buffer.

If this function is called interactively, or if SWITCH is
non-nil, this switches to the newly created buffer.
Otherwise, this just returns the newly created buffer."
  (interactive "sBuffer name: \nP")
  (let* ((newbuf (concat "*" buf "*"))
	 (newbuf (get-buffer-create newbuf)))
    (if (or (called-interactively-p 'any) switch)
	(switch-to-buffer newbuf)
      newbuf)))

;; ---

(defun revert-all-buffers ()
  "Reverts all buffers, including special buffers.
This reverts all buffers in like manner to `revert-buffer'.  The user is NOT
asked to confirm, so be careful when using this function.  IGNORE-AUTO and
PREVERSE-MODES are the same as for `revert-buffer', and they are specified
as prefix args."
  (interactive)
  (let (buf bufname idx)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(setq bufname (buffer-name buf))
	(when (and (buffer-file-name)
		   (file-exists-p (buffer-file-name))
		   (not (buffer-modified-p)))
	  (revert-buffer t t))))
    (message "Reverted all file buffers")))

(defun narrow-to-region2 (start end)
  "Call `narrow-to-region' with START and END."
  (interactive "r")
  (narrow-to-region start end)
  (setq deactivate-mark t))

(defun view-into-buffer (base-buffer &optional clone)
  "Create an indirect buffer and show it in another window.

Creates an indirect buffer of BASE-BUFFER and shows it in
another window.  BASE-BUFFER should be a live buffer or the
name of an existing buffer.  If the optional arg CLONE is
non-nil, BASE-BUFFER's state is preserved in the indirect
buffer, meaning things like minor and major modes.  Otherwise,
the indirect buffer's state is reset to default values.

Interactively, CLONE is a prefix argument."
  (interactive "bMake indirect buffer of: \nP")
  (let ((view-buffer (format "*view into %s*" base-buffer))
	mode)
    (make-indirect-buffer base-buffer view-buffer)
    (split-window-sensibly)
    (switch-to-buffer view-buffer)
    (with-current-buffer base-buffer
      (setq mode major-mode))
    (funcall mode)))
