;; -*- lexical-binding: t; -*-

(require 'desktop)

(eval-and-compile
  (defgroup desktop-ext nil
    "Group for Desktop extensions."
    :group 'user-extensions))

(defconst user-ext-desktop-prefix "C-c M-d"
  "Prefix for desktop-related commands.")

(global-set-key (kbd (concat user-ext-desktop-prefix " s")) #'desktop-save-mode)
(global-set-key (kbd (concat user-ext-desktop-prefix " l")) #'desktop-read)
(global-set-key (kbd (concat user-ext-desktop-prefix " M-c")) #'desktop-clear)

(provide 'desktop-ext)
