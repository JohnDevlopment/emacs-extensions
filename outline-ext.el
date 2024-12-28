;;; outline-ext.el --- Outline mode extension.       -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John

;; Author: John <john@john-System-Product-Name>
;; Keywords: extensions

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

(eval-when-compile
  (require 'outline))

(define-key outline-mode-map (kbd "<S-return>") #'outline-insert-heading)

;;;###autoload
(defun outline--extra-hook ()
  t)

;;;###autoload
(add-hook 'outline-mode-hook #'outline--extra-hook)

(provide 'outline-ext)

;;; outline-ext.el ends here
