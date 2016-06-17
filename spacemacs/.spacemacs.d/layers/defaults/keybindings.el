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
  "."   'spacemacs/alternate-buffer
  ","   'evil-avy-goto-char-2

  ;; I don't need align-repeat, that is why evil-repeat exists
  "xar" 'align-regexp

  ;; mnemonic of Quit Window
  "qw"  'evil-quit

  "wV"  'split-window-right
  "wv"  'split-window-right-and-focus
  "wS"  'split-window-below
  "ws"  'split-window-below-and-focus)

(bind-map-set-keys evil-normal-state-map "Q" 'fill-paragraph)

(define-key evil-ex-map "e " 'helm-find-files)
(define-key evil-ex-map "b " 'helm-buffers-list)

(define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)
