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
  "TAB"   'evil-jump-item
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

(global-set-key (kbd "C-,") 'helm-M-x)

(bind-map-set-keys evil-normal-state-map
  "Q" 'fill-paragraph)
