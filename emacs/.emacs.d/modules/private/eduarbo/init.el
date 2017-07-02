;;; private/eduarbo/init.el -*- lexical-binding: t; -*-

(setq user-mail-address "eduarbo@gmail.com"
      user-full-name    "Eduardo Ruiz Macias")

(setq doom-leader-key ","
      doom-localleader-key ", m")

(setq +doom-modeline-height 25
      +present-big-font (font-spec :family "Hack" :size 18)
      doom-font (font-spec :family "Hack" :size 12)
      doom-variable-pitch-font (font-spec :family "Hack" :size 12)
      doom-unicode-font (font-spec :family "Hack" :size 12)
      doom-line-number-lpad 3)

;; Set my notes directory
(setq +org-dir (expand-file-name "~/Google Drive/org/"))

(doom/toggle-fullscreen)
