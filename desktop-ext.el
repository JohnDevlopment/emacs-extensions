;; -*- lexical-binding: t; -*-

(defgroup desktop-ext nil
  "Group for Desktop extensions."
  :group 'user-extensions)

(eval-and-compile
  (defconst user-ext-desktop-prefix "C-c M-d"
    "Prefix for desktop-related commands."))

(unless (featurep 'jdesktop)
  (keymaps-ext-set-keymap-global
   (eval-when-compile (concat user-ext-desktop-prefix " s"))
   #'desktop-save-mode)
  (keymaps-ext-set-keymap-global
   (eval-when-compile (concat user-ext-desktop-prefix " l"))
   #'desktop-read)
  (keymaps-ext-set-keymap-global
   (eval-when-compile (concat user-ext-desktop-prefix " M-c"))
   #'desktop-clear))

(extension-provide 'desktop-ext)
;;; desktop-ext.el ends here
