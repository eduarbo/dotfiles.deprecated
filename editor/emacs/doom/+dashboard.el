;;; ~/.dotfiles/editor/emacs/doom/banners.el -*- lexical-binding: t; -*-

(setq +doom-dashboard-functions
      '(eduarbo-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

(defun mi-dashboard-widget-banner ()
  "Insert the ascii banner contain in file and center it in the window.
FILE: the path to the file containing the banner."
  (let* ((banner-width 0)
         (banner (with-temp-buffer
                   (insert-file-contents (eduarbo--get-banner-path "momacs"))
                   (while (not (eobp))
                     ;; Get max line lenght
                     (let ((line-length (- (line-end-position) (line-beginning-position))))
                       (if (< banner-width line-length)
                           (setq banner-width line-length)))
                     (forward-line 1))
                   (buffer-string))))
    (let ((margin (max 0 (floor (/ (- +doom-dashboard--width banner-width) 2)))))
      (mapc (lambda (line)
              (insert (propertize (concat (make-string margin ?\s) line) 'face 'font-lock-comment-face) " ")
              (insert "\n"))
            (split-string banner "\n" t)))))
