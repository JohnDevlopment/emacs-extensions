;; -*- lexical-binding: t; -*-

(--ignore :no-warn
  (defgroup desktop-ext nil
    "Group for Desktop extensions."
    :group 'user-extensions)
  t)

(defconst user-ext-desktop-prefix "C-c M-d"
  "Prefix for desktop-related commands.")

(unless (featurep 'jdesktop)
  (global-set-key (kbd (concat user-ext-desktop-prefix " s")) #'desktop-save-mode)
  (global-set-key (kbd (concat user-ext-desktop-prefix " l")) #'desktop-read)
  (global-set-key (kbd (concat user-ext-desktop-prefix " M-c")) #'desktop-clear))

(extension-provide 'desktop-ext)
;;; desktop-ext.el ends here
