;;; server-view-ext.el --- View a file in server mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John

;; Author: John <john@john-System-Product-Name>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-and-compile
  (defvar user-ext-current-server-view-buffer nil
    "The buffer that is currently viewed by `server-ext-view-file'."))

(defun server-ext-view-file (file)
  "View FILE.
Called via the Emacs server by emacsclient."
  (interactive)
  (unless (file-exists-p file)
    (user-error "%s does not exist" file))
  (let ((had-a-buf (get-file-buffer file))
	(buffer (find-file-noselect file)))
    ;; We want the buffer saved to a variable
    (view-buffer buffer (and (not had-a-buf)
			     #'kill-buffer-if-not-modified))
    (setq user-ext-current-server-view-buffer buffer)
    (with-current-buffer buffer
      (local-set-key (kbd "C-x #") #'server-ext-quit)
      (setq header-line-format "When done with this file, type C-x #")
      (message "When done with this file, type C-x #"))))

(defun server-ext-quit ()
  "Quit the currently viewed buffer."
  (interactive)
  (View-quit)
  (delete-frame)
  (setq user-ext-current-server-view-buffer nil))

(provide 'server-view-ext)
;;; server-view-ext.el ends here
