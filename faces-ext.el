;;; faces-ext.el --- Custom faces                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John
;; Author: John <john@john-System-Product-Name>
;; Keywords:
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

;;; Custom variables and faces

(defface font-lock-operator-face
  '((t (:inherit font-lock-function-name-face)))
  "Font lock mode face used to highlight operators."
  :group 'font-lock-faces)

(defface log-view-info
  '((t (:foreground "Blue1" :weight bold)))
  "Face for DEBUG in log files."
  :group 'user-extensions)

(defface log-view-debug
  '((t (:inherit success)))
  "Face for DEBUG in log files."
  :group 'user-extensions)

(defface log-view-error
  '((t (:inherit error :weight bold)))
  "Face for ERROR in log files"
  :group 'user-extensions)

(defface log-view-warning
  '((t (:inherit warning :weight bold)))
  "Face for WARNING in log files"
  :group 'user-extensions)

(defface log-view-date
  '((t (:foreground "red")))
  "Face for date strings in log files."
  :group 'user-extensions)

(defface log-view-time
  '((t (:inherit org-agenda-current-time)))
  "Face for time strings in log files."
  :group 'user-extensions)
