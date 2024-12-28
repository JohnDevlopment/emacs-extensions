;;; ibuffer-ext.el --- Extension for IBuffer         -*- lexical-binding: t; -*-

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
  (require 'ibuffer))

(require 'alist-ext)

(advice-add 'ibuffer-do-revert :after #'ibuffer--after-operation
	    (alist-ext-define 'name "after-revert"))

(advice-add 'ibuffer-do-view :after
	    (lambda (&rest args)
	      (interactive)
	      ;; (message "Current buffer: %s" (current-buffer))
	      (view-mode))
	    (alist-ext-define 'name "after-view"))

(define-key ibuffer-mode-map (kbd "/ T") #'ibuffer-toggle-current-filter-group)

(defun ibuffer--after-operation ()
  (ibuffer-unmark-all-marks))

;;;###autoload
(defun ibuffer-toggle-current-filter-group ()
  "Expand/collapse the filter group of point.
If point is on a buffer, toggle the group it is in."
  (interactive)
  (unless (char-equal (char-after) ?\[)
    (ibuffer-backward-filter-group))
  (ibuffer-toggle-filter-group))

;;;###autoload
(defun ibuffer--extra-hook ()
  (1+ 1))

;;;###autoload
(add-hook 'ibuffer-mode-hook #'ibuffer--extra-hook)

(provide 'ibuffer-ext)
;;; ibuffer-ext.el ends here
