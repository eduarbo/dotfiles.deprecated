;;; config.el --- notes layer packages file for Spacemacs.
;;
;; Copyright (C) 2016  Eduardo Ruiz
;;
;; Author: Eduardo Ruiz <eduarbo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defvar notes-secrets-filename "psst.org"
  "Secrets file name")

(defvar notes-secrets-path (concat deft-directory notes-secrets-filename)
  "Path to my secrets file")
