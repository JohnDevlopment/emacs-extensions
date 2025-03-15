;; -*- lexical-binding: t; -*-

(require 'desktop)
(require 'use-package)

(eval-and-compile
  (defgroup desktop-ext nil
    "Group for Desktop extensions."
    :group 'user-extensions)

  (defcustom user-ext-desktop-load-path ""
    "Load path for `use-package' of package `jdesktop'."
    :type 'file
    :group 'desktop-ext)

  (defconst user-ext-desktop-prefix "C-c M-d"
    "Prefix for desktop-related commands."))

(global-set-key (kbd (concat user-ext-desktop-prefix " s")) #'desktop-save-mode)
(global-set-key (kbd (concat user-ext-desktop-prefix " l")) #'desktop-read)
(global-set-key (kbd (concat user-ext-desktop-prefix " M-c")) #'desktop-clear)

(use-package jdesktop
  :if (locate-library 'jdesktop)
  :defer 1
  :config
  (jdesktop-default-bindings))

(provide 'desktop-ext)
