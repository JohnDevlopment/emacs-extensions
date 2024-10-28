(eval-and-compile
  (require 'tcl))

;; make function called tcl--extra-hook and add it to tcl-mode-hooks
(defun tcl--extra-hook ()
  "Extra hook for `tcl mode'."
  (setq tcl-indent-level 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq indent-line-function #'tcl-indent-line)
  (define-key tcl-mode-map [remap tcl-eval-region] #'tcl-eval-region2))

(defun tcl-eval-region2 (start end &optional and-go)
  "Send the current region to the inferior Tcl process.
Prefix argument means switch to the Tcl buffer afterwards.

Internally, this calls `tcl-eval-region' and deactivates the
mark afterwards."
  (interactive "r\nP")
  (tcl-eval-region start end and-go)
  (deactivate-mark nil))

(add-hook 'tcl-mode-hook #'tcl--extra-hook)
