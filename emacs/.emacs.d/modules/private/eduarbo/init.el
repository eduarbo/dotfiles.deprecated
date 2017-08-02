;;; private/eduarbo/init.el -*- lexical-binding: t; -*-

(setq user-mail-address "eduarbo@gmail.com"
      user-full-name    "Eduardo Ruiz Macias")

(setq doom-leader-key ","
      doom-localleader-key ",m")

(setq +doom-modeline-height 25
      +present-big-font (font-spec :family "Hack" :size 18)
      doom-font (font-spec :family "Hack" :size 12)
      doom-variable-pitch-font (font-spec :family "Hack" :size 12)
      doom-unicode-font (font-spec :family "Hack" :size 12)
      doom-line-number-lpad 3)

;; Set my notes directory
(setq +org-dir (expand-file-name "~/Google Drive/org/"))
(setq +present--original-font doom-font)

;; Enable accents
(setq ns-alternate-modifier 'none)

;; JavaScript
(setq js2-bounce-indent-p t
      ;; Let flycheck handle parse errors
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil)

;; Force everything to indent 2 spaces
(setq-default tab-width 2)
(setq js-indent-level tab-width
      js2-basic-offset tab-width

      css-indent-offset tab-width
      web-mode-markup-indent-offset tab-width
      web-mode-css-indent-offset tab-width
      web-mode-code-indent-offset tab-width
      web-mode-attr-indent-offset tab-width

      sh-indentation tab-width
      sh-basic-offset tab-width)

;; Get some context
(setq scroll-margin 10)
