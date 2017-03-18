;;; keybindings.el --- defaults layer packages file for Spacemacs.
;;
;; Copyright (C) 2016  Eduardo Ruiz
;;
;; Author: Eduardo Ruiz <eduarbo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs/set-leader-keys
  "SPC"   'helm-mini
  "hd,"   'my/describe-foo-at-point
  "TN"    'my/cycle-available-themes
  "n TAB" 'my/narrow-or-widen-dwim
  "cb"    'my/comment-box
  ";"     'eval-expression
  "."     'spacemacs/alternate-buffer
  "wV"    'split-window-right
  "wv"    'split-window-right-and-focus
  "wS"    'split-window-below
  "ws"    'split-window-below-and-focus)

(bind-map-set-keys evil-normal-state-map
  "TAB"   'evil-jump-item
  "Q"     'fill-paragraph)

(bind-map-set-keys evil-visual-state-map
  "TAB"   'evil-jump-item)
