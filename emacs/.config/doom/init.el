;;; private/eduarbo/init.el -*- lexical-binding: t; -*-

;; Initialize in fullscreen
(toggle-frame-fullscreen)

;; I've swapped these keys on my keyboard
(setq user-mail-address "eduarbo@gmail.com"
      user-full-name    "Eduardo Ruiz Macias"

      ;; Set my notes directory
      +org-dir (expand-file-name "~/Google Drive/org/")

      doom-font (font-spec :family "Hack" :size 14)
      doom-variable-pitch-font (font-spec :amily "Fira Sans")
      doom-unicode-font (font-spec :family "Hack" :size 14)
      doom-big-font (font-spec :family "Hack" :size 19)

      ;; Enable accents
      ns-alternate-modifier 'none
      ;; Get some context when scrolling
      scroll-margin 10
      ;; disable line numbers
      doom-line-numbers-style nil
      ;; use gnu ls to allow dired to sort directories
      insert-directory-program "gls" dired-use-ls-dired t

      org-ellipsis " ▼ ")


;;
;; Disable packages
;;

;; I don't need evil-escape
(def-package-hook! evil-escape :disable)

; Use my own snippets
(def-package-hook! emacs-snippets :disable)
