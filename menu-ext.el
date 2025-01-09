;;; menu-ext.el --- menu extension -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John
;; Author: John
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defmacro define-menu (map key name)
  "Define a menu for a keymap MAP.
KEY is a symbol used to name the key for the menu.  It
should not be quoted. NAME is a string title for the menu."
  (declare (indent 2))
  `(define-key ,map [menu-bar ,key]
     (cons ,name (make-sparse-keymap ,name))))

(defmacro define-menu-item (map key name command &rest args)
  (declare (indent 2))

  `(define-key ,map [menu-bar ,@key]
     '(,name . ,command)))

;; Make a menu keymap (with a prompt string)
;; and make it the menu bar item’s definition.
(define-key global-map [menu-bar words]
  (cons "Words" (make-sparse-keymap "Words")))


;; Define specific subcommands in this menu.
(define-key global-map
  [menu-bar words forward]
  '("Forward word" . forward-word))

(define-key global-map
  [menu-bar words backward]
  '("Backward word" . backward-word))
