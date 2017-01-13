;;; keybindings.el --- notes layer packages file for Spacemacs.
;;
;; Copyright (C) 2016  Eduardo Ruiz
;;
;; Author: Eduardo Ruiz <eduarbo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs/set-leader-keys
  "nj" 'notes/open-today-journal
  "ny" 'notes/open-yesterday-journal
  "nt" 'notes/open-tomorrow-journal
  "nS" 'notes/open-darkest-secrets)
