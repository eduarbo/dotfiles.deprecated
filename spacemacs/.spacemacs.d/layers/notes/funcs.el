;;; funcs.el --- notes layer packages file for Spacemacs.
;;
;; Copyright (C) 2016  Eduardo Ruiz
;;
;; Author: Eduardo Ruiz <eduarbo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun notes-journal-path ()
  "Path to the note of the day"
  (concat notes-directory (format-time-string "%Y%m%d.org")))

(defun notes-open-journal ()
  "Open note of the day"
  (interactive)
  (find-file (notes-journal-path)))

(defun notes-open-darkest-secrets ()
  "Open my secrets file"
  (interactive)
  (find-file notes-secrets-path))

(defun notes-journal-file-insert ()
  "Insert's the journal heading based on the file's name."
  (interactive)
  (when (string-match "\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
                      (buffer-name))
    (let ((year  (string-to-number (match-string 1 (buffer-name))))
          (month (string-to-number (match-string 2 (buffer-name))))
          (day   (string-to-number (match-string 3 (buffer-name))))
          (datim nil))
      (setq datim (encode-time 0 0 0 day month year))
      (insert (format-time-string
               "#+TITLE: Journal - %A, %b %e, %Y\n" datim)
              "#+PROPERTY: LOGGING lognoterepeat\n\n"))))
