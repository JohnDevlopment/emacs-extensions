;;; -*- lexical-binding: t; -*-

(require 'nroff-mode)

;;; Code:

;; Add 1p and 3p to nroff-mode auto mode alist
(add-to-list 'auto-mode-alist '("\\.[13]p\\'" . nroff-mode))
