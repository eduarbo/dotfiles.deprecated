;;; private/eduarbo/init.el -*- lexical-binding: t; -*-

;; Special file that is automatically loaded after Doom core files, but before
;; modules are loaded. Use it to configure DOOM.

(setq doom-leader-key ","
      doom-localleader-key "SPC" ;; FIXME

      ;; Set my notes directory
      +org-dir (expand-file-name "~/Google Drive/org/")

      user-mail-address "eduarbo@gmail.com"
      user-full-name    "Eduardo Ruiz Macias")

;; Fonts
(set! :font "Hack" :size 14)
(set! :big-font "Hack" :size 16)
;; (set! :variable-font "Fira Sans" :size 12)
(set! :unicode-font "Hack" :size 14)

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)

;; Doom theme
(setq doom-theme 'doom-one)

;; Enable accents
(setq ns-alternate-modifier 'none)

;; JavaScript defaults
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

;; Get some context when scrolling
(setq scroll-margin 10)
