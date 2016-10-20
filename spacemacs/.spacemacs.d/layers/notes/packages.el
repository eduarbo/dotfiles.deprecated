;;; packages.el --- notes layer packages file for Spacemacs.
;;
;; Copyright (C) 2016  Eduardo Ruiz
;;
;; Author: Eduardo Ruiz <eduarbo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst notes-packages
  '((org :location built-in)
    deft))

(defun notes/post-init-deft ()
  (setq deft-recursive t
        deft-directory notes-directory
        deft-use-filename-as-title nil
        deft-auto-save-interval 0
        deft-org-mode-title-prefix t
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase)))

  (spacemacs|add-toggle deft-toggle-recursive
    :status deft-recursive
    :on (setq deft-recursive t) (deft-refresh)
    :off (setq deft-recursive nil) (deft-refresh)
    :documentation "Toggle recursively search for files in subdirectories"
    :evil-leader-for-mode (deft-mode . "t"))

  (spacemacs/set-leader-keys
    "nd" 'deft
    "nn" 'deft-find-file)

  (with-eval-after-load 'deft
    (bind-map-set-keys deft-mode-map
      "<S-return>" 'deft-new-file)
    (evil-define-key 'normal deft-mode-map
      "q" 'quit-window)
    (spacemacs/set-leader-keys-for-major-mode "deft-mode"
      "a" 'deft-archive-file
      "c" 'deft-filter-clear
      "s" 'deft-toggle-sort-method)))

(defun notes/post-init-org ()
  (setq org-capture-templates nil)
  ;; Wrap lines
  (spacemacs/add-to-hooks 'spacemacs/toggle-auto-fill-mode-on
                          '(org-mode-hook))
  ;; Break lines automatically
  (spacemacs/add-to-hooks 'spacemacs/toggle-visual-line-navigation-on
                          '(org-mode-hook))
  ;; Distinguish wrapped lines with curly arrows
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

  (let ((templates
         '(("s" "Secret"
            entry (file notes-secrets-path)
            "* %? :crypt:%^g\n")
           ("l" "Login"
            entry (file notes-secrets-path)
            "* %? :crypt:login:\n %^{username}p\n %^{password}p\n %^{website}\n"))))
    (when (file-exists-p notes-secrets-path)
      (setq org-capture-templates (append templates))))

  (setq org-agenda-files (list notes-directory)
        org-default-notes-file (notes/journal-path)))
