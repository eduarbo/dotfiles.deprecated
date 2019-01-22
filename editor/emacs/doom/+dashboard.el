;;; ~/.dotfiles/editor/emacs/doom/banners.el -*- lexical-binding: t; -*-

(setq +doom-dashboard-functions
      '(eduarbo-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

(defun eduarbo--get-banner-path (name)
  "Return the full path to banner with name."
  (concat (DIR!) "banners/" (format "%s.txt" name)))

;; (defun eduarbo-dashboard-widget-banner ()
;;   (let ((point (point)))
;;     (mapc (lambda (line)
;;             (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
;;                                 'face 'font-lock-comment-face) " ")
;;             (insert "\n"))
;;           (insert-file-contents (eduarbo--get-banner-path "momacs")))))

(defun eduarbo-dashboard-widget-banner ()
  "Insert the ascii banner contain in file and center it in the window.
FILE: the path to the file containing the banner."
  (insert
   (with-temp-buffer
     (insert-file-contents (eduarbo--get-banner-path "momacs"))
     (let ((banner-width 0))
       (while (not (eobp))
         (let ((line-length (- (line-end-position) (line-beginning-position))))
           (if (< banner-width line-length)
               (setq banner-width line-length)))
         (forward-line 1))
       (goto-char 0)
       (let ((margin (max 0 (floor (/ (- +doom-dashboard--width
                                         banner-width) 2)))))
         (while (not (eobp))
           (insert (make-string margin ?\s))
           (forward-line 1))))
     (buffer-string))))

(defun eduarbo-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'font-lock-comment-face) " ")
            (insert "\n"))
          '("                       ▄▄▄▄              ▄           "
            "                        ▌▄ █▄          ▄▀▄▀▄         "
            "                        ▌███ ▌ ▄▄▄▄▄▄ ▐ ███ ▌        "
            "                      ▄▄▀ ░░ ▐░░▐░░░▐ ▀▄ ░░░ ▌▄      "
            "                    ▄▀  ░░░░ ▐░░ ▌░▌░░ ▐ ░░░░░ ▀▄    "
            "                   ▐  ░░░░░  ▐░░░▐▌░░░▐ ░░░░░░░░ ▌   "
            "                  ▐ ░░░ ▄▀▀▄  ░░░░░░░ ▄▀▀▄ ░░░░░░ ▌  "
            "                  ▐  ░░ ▀▄▄▀   ░░░░░  ▀▄▄▀ ░░░░   ▌  "
            "                --▐---------___▄▀▀▄__-----------  ▌  "
            "                 _▐_________--—▀▄▄▀--__________   ▌  "
            "                   ▀▌ ░░░░░   ▄▄▌▐▄▄   ░░░░░░░░ ▐▀   "
            "                    ▀▌ ░░  ▐▀▀      ▀▀▌  ░░░░ ▐▀     "
            "                      ▀▌░                 ░▐▀▀       "
            "                        ▀▀▄▄▄▄▄▄▄▄▄▄▄▄▄▄ ▀▀          "
            "                               ▐     ▌               "
            "                             ▄▄▌     ▐▄▄             "
            "                          ▄▄▀    ░░░    ▀▄▄          "
            "                        ▄▄▌   ░░░░░░░░    ▐▄▄        "
            "                        ▌   ░░░░░░░░░░░░░   ▐        "
            ""
            "     ▄      ▄     | ███╗   ███╗ ██████╗ ███╗   ███╗ █████╗  ██████╗███████╗"
            "  ▄ ░░▄▄▀▀▄▄░░ ▄  | ████╗ ████║██╔═══██╗████╗ ████║██╔══██╗██╔════╝██╔════╝"
            " ░░ ▄▀ ░░░░ ▀▄ ░░ | ██╔████╔██║██║   ██║██╔████╔██║███████║██║     ███████╗"
            "   ▐░░░░░░░░░░▌   | ██║╚██╔╝██║██║   ██║██║╚██╔╝██║██╔══██║██║     ╚════██║"
            "    ▀▄▄▄▄▄▄▄▄▀    | ██║ ╚═╝ ██║╚██████╔╝██║ ╚═╝ ██║██║  ██║╚██████╗███████║"
            "                  | ╚═╝     ╚═╝ ╚═════╝ ╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝"))))
