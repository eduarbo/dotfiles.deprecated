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
  (spacemacs/set-leader-keys
    "nd" 'notes-open-today-journal
    "ny" 'notes-open-yesterday-journal
    "nt" 'notes-open-tomorrow-journal
    "nn" 'deft
    "nl" 'deft-find-file
    "nS" 'notes-open-darkest-secrets)

  (with-eval-after-load 'deft
    (define-key deft-mode-map [(shift return)] 'deft-new-file)
    (spacemacs/set-leader-keys-for-major-mode "deft-mode"
      "a" 'deft-archive-file
      "t" 'deft-toggle-recursive-search
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

  (let ((templates
         '(("t" "Todo"
            entry (file+headline (notes-journal-path) "Tasks")
            "* TODO %?\n\n%i\n")
           ("r" "Reminder"
            entry (file+headline (notes-journal-path) "Tasks")
            "* TODO %?\n%^{prompt|SCHEDULED|DEADLINE}: %^t\n\n%i\n")
           ("j" "Journal"
            entry (file (notes-journal-path))
            "* %? :journal:\n%T\n\n%i\n"
            :empty-lines 1))))
    (setq org-capture-templates (append templates org-capture-templates)))

  (setq org-agenda-files (list deft-directory)
        org-default-notes-file (notes-journal-path))

  (require 'autoinsert)
  (setq auto-insert-query nil)  ;; don't want to be prompted before insertion
  (add-hook 'find-file-hook 'auto-insert)
  (add-to-list 'auto-insert-alist '(".*/[0-9]*\.org$" . notes-journal-file-insert)))
