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
  "SPC"   'avy-goto-char-2
  "n TAB" 'my/narrow-or-widen-dwim
  "cb"    'my/comment-box
  ";"     'eval-expression
  "wV"    'split-window-right
  "wv"    'split-window-right-and-focus
  "wS"    'split-window-below
  "ws"    'split-window-below-and-focus)

(bind-map-set-keys evil-motion-state-map
  "L"   'evil-forward-arg
  "H"   'evil-backward-arg
  "C-h" 'evil-window-left
  "C-j" 'evil-window-down
  "C-k" 'evil-window-up
  "C-l" 'evil-window-right)

(bind-map-set-keys evil-normal-state-map
  ":"       'evil-repeat-find-char-reverse
  "L"       'evil-forward-arg
  "H"       'evil-backward-arg
  "Q"       'fill-paragraph
  "<C-tab>" 'evil-jump-item)
