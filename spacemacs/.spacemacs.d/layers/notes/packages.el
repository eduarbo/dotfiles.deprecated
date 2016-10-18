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
    (org-crypt :location built-in)
    deft))

(defun notes/post-init-deft ()
  (setq deft-directory notes-directory)

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
    (define-key deft-mode-map [(shift return)] 'deft-new-file)
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
