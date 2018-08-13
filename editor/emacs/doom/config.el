;;; ~/.dotfiles/editor/emacs/doom/config.el -*- lexical-binding: t; -*-

(load! "+bindings")

(setq
 ;; A E S T H E T I C
 doom-font (font-spec :family "Hack" :size 12)
 doom-big-font (font-spec :size 19)
 ;; That's me!!!
 user-mail-address "eduarbo@gmail.com"
 user-full-name    "Eduardo Ruiz Macias"
 ;; FIXME prefer ag over rg as the latter also matches the filenames and
 ;; triggers the search at the first character so it becomes really slow when
 ;; you start typing
 +helm-project-search-engines '(ag rg pt)
 helm-ag-command-option "--hidden"
 ;; Set my notes directory
 org-directory (expand-file-name "~/Google Drive/org/")
 ;; Enable accents
 ns-alternate-modifier 'none
 ;; Get some context when scrolling
 scroll-margin 10
 ;; disable line numbers
 doom-line-numbers-style nil
 ;; use gnu ls to allow dired to sort directories
 insert-directory-program "gls" dired-use-ls-dired t
 ;; Given ~/Projects/FOSS/emacs/lisp/comint.el => emacs/lisp/comint.el
 +doom-modeline-buffer-file-name-style 'relative-from-project
 ;; ... is boring
 org-ellipsis " ▼ ")

;; set defaults for buffer variables
(setq-default
 ;; default to flyspell prog mode
 flyspell-generic-check-word-predicate #'flyspell-generic-progmode-verify
 ;; Make it easy to identify trailing whitespace
 show-trailing-whitespace t)
