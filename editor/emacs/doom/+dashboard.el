;;; ~/.dotfiles/editor/emacs/doom/banners.el -*- lexical-binding: t; -*-
(defvar +mi-dashboard-banner "momacs")

(setq +doom-dashboard-functions
      '(+mi-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

(defun +mi-dashboard-widget-banner ()
  "Insert ASCII banner contained in file and center it."
  (let* ((banner-lines (split-string (+mi--dashboard-get-banner) "\n"))
         (banner-width (seq-reduce #'+mi--dashboard-get-banner-width banner-lines 0))
         (margin (max 0 (floor (/ (- +doom-dashboard--width banner-width) 2)))))
    (mapc (lambda (line)
            (insert (propertize (concat (make-string margin ?\s) line)
                                'face 'font-lock-comment-face) "\n"))
          banner-lines)))


;;
;; Helpers

(defun +mi--dashboard-get-banner ()
  "Load banner from file and return as a string."
  (condition-case _
      (with-temp-buffer
        (insert-file-contents
         (concat doom-private-dir "banners/" (format "%s.txt" +mi-dashboard-banner)))
        (buffer-string))
    (file-error "")))

(defun +mi--dashboard-get-banner-width (banner-width line)
  "Get length of longest line"
  (if (< banner-width (length line))
      (length line)
    banner-width))
