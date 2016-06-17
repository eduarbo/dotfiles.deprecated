;;; config.el --- defaults layer packages file for Spacemacs.
;;
;; Copyright (C) 2016  Eduardo Ruiz
;;
;; Author: Eduardo Ruiz <eduarbo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Workaround to get rid of annoying completion-at-point in empty strings
(setq tab-always-indent t)
;; Follow symlinks
(setq vc-follow-symlinks t)

;; Set space as a delimiter arguments for lisp-family languages
(defun set-lispish-evil-args-delimiters ()
  "Override default args delimiters on lisp languages"
  (setq-local evil-args-delimiters '(" ")))

(add-hook 'lisp-mode-hook 'set-lispish-evil-args-delimiters)
(add-hook 'emacs-lisp-mode-hook 'set-lispish-evil-args-delimiters)

(defun my//include-underscores-in-word-motions ()
  "Include underscores in word motions"
  (modify-syntax-entry ?_ "w")
  )
(add-hook 'python-mode-hook #'my//include-underscores-in-word-motions)
(add-hook 'ruby-mode-hook #'my//include-underscores-in-word-motions)
(add-hook 'js2-mode-hook #'my//include-underscores-in-word-motions)

;; Threat the underscore char as a symbol constituent for everything except for
;; the inner-word text object, so `ciw' will threat the _ as part of the word
;; and `caw' will threat it as a symbol
;; (defadvice evil-inner-word (around underscore-as-word activate)
;;   (let ((table (copy-syntax-table (syntax-table))))
;;     (modify-syntax-entry ?_ "w" table)
;;     (with-syntax-table table ad-do-it)))
