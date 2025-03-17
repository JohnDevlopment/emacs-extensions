;; *- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (declare-function jdesktop-default-bindings "jdesktop"))

(eval-and-compile
  (defun user-ext-desktop-load-path--setter (symbol value)
    (set-default symbol value)
    (load-jdesktop-package))

  (defcustom user-ext-desktop-load-path ""
    "Load path for `use-package' of package `jdesktop'."
    :type 'file
    :set #'user-ext-desktop-load-path--setter
    :group 'desktop-ext))

(defun load-jdesktop-package ()
  "Load `jdesktop' package."
  (let ((path user-ext-desktop-load-path))
    (cl-ext-when (file-exists-p path)
      (add-to-list 'load-path path))
    (use-package jdesktop
      :if (locate-library "jdesktop")
      :defer t
      :functions
      jdesktop-default-bindings
      :config
      (jdesktop-default-bindings))))

;; (cl-prettyprint (symbol-function 'load-jdesktop-package))

(provide 'jdesktop-bootstrap)
;;; jdesktop-bootstrap.el ends here
