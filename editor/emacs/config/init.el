;;; private/eduarbo/init.el -*- lexical-binding: t; -*-

;; Initialize in fullscreen
(toggle-frame-fullscreen)

(let ((font "Hack"))
  (setq
   doom-font (font-spec :family font :size 14)
   doom-big-font (font-spec :family font :size 19)
   doom-unicode-font (font-spec :family font :size 15)
   doom-variable-pitch-font (font-spec :family "Fira Sans")
   ivy-posframe-font (font-spec :family font :size 16)
   ivy-height 12))

(setq user-mail-address "eduarbo@gmail.com"
      user-full-name    "Eduardo Ruiz Macias"

      ;; Set my notes directory
      +org-dir (expand-file-name "~/Google Drive/org/")

      ;; Enable accents
      ns-alternate-modifier 'none
      ;; Get some context when scrolling
      scroll-margin 10
      ;; disable line numbers
      doom-line-numbers-style nil
      ;; use gnu ls to allow dired to sort directories
      insert-directory-program "gls" dired-use-ls-dired t

      +doom-modeline-buffer-file-name-style 'relative-from-project
      show-trailing-whitespace t

      org-ellipsis " ▼ ")


;;
;; Disable packages
;;

; Use my own snippets
(def-package-hook! emacs-snippets :disable)
(def-package-hook! lsp-mode :disable)
(def-package-hook! lsp-javascript-typescript :disable)
