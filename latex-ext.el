;;; -*- lexical-binding: t; -*-

(defun user-ext-cdlatex-mode-extra-hook ()
  "Extra hook for `cdlatex-mode'.")

(add-hook 'cdlatex-mode-hook #'user-ext-cdlatex-mode-extra-hook)
