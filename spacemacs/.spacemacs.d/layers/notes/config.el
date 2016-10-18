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

(defvar notes-directory (expand-file-name "~/Google Drive/notes/")
  "Path to my notes directory")

(defvar notes-journal-extension ".org"
  "Journal extension")

(defvar notes-journal-filepath-format "journal/%Y/%Y-%m-%d"
  "Format the journal's file path. Do not include extension.
See help for function `format-time-string` to see available constructs")

(defvar notes-journal-title-format "#+TITLE: Journal - %A, %b %e, %Y\n"
  "Format the journal's title.
See help for function `format-time-string` to see available constructs")

(defvar notes-secrets-filename "psst.org"
  "Secrets file name")

(defvar notes-secrets-path (concat notes-directory notes-secrets-filename)
  "Path to my secrets file")
