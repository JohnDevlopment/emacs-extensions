;; -*- lexical-binding: t; -*-

(require 'perl-mode)

(defun perl-mode--extra-hook ())

(define-scratch-buffer-function perl-ext-scratch-buffer "perl scratch" nil
  "Create a Perl scratch buffer."
  nil
  (perl-mode))

;; M-SPC to do completion
(define-key perl-mode-map (kbd "M-SPC") 'completion-at-point)

(add-hook 'perl-mode-hook #'perl-mode--extra-hook)
