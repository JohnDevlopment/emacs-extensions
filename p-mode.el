;;; p-mode.el --- Globalized minor mode for electric-pair-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 John Russell

;; Author: John Russell <john@john-System-Product-Name>

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

;;; Code:

(defcustom user-ext-electric-pair-global-modes '(not)
  "List of global modes in which `electric-pair-local-mode' should be
enabled."
  :type '(choice
	  (const :tag "all" t)
	  (set :tag "modes" :value (not)
	       (const :tag "Except" not)
	       (repeat :inline t (symbol :tag "Mode"))))
  :group 'user-ext-global-modes)

(defun electric-pair-mode-turn-on ()
  "Turn on electric pair mode"
  (interactive)
  (when (cond ((eq (car-safe user-ext-electric-pair-global-modes) 'not)
	       (not (memq major-mode (cdr user-ext-electric-pair-global-modes))))
	      (t
	       (memq major-mode user-ext-electric-pair-global-modes)))
    (electric-pair-local-mode 1)))

(define-globalized-minor-mode global-electric-pair-mode electric-pair-mode electric-pair-mode-turn-on)
