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

(defun notes/pre-init-org ()
  ;; mnemonic of Journal
  (spacemacs/set-leader-keys
    "aj" 'notes-open-journal
    "aS" 'notes-open-darkest-secrets))

(defun notes/post-init-org ()
  (setq org-capture-templates nil)
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

  (setq org-agenda-files (list notes-directory)
        org-default-notes-file (notes-journal-path))

  (require 'autoinsert)
  (setq auto-insert-query nil)  ;; don't want to be prompted before insertion
  (add-hook 'find-file-hook 'auto-insert)
  (add-to-list 'auto-insert-alist '(".*/[0-9]*\.org$" . notes-journal-file-insert)))


(defun notes/post-init-deft ()
  (with-eval-after-load 'deft
    (setq deft-use-filename-as-title nil
          ;; Disabled until find a way to disable it by buffer. deft-mode-hook
          ;; is fired after timer is created and when deft-mode is called it
          ;; kills all the local variables :/
          ;; TODO contribute with a better implementation
          deft-auto-save-interval 0
          deft-directory notes-directory
          deft-use-filter-string-for-filename t
          deft-file-naming-rules '((noslash . "_")
                                   (nospace . "_")
                                   (case-fn . downcase)))

    (define-key deft-mode-map [(shift return)] 'deft-new-file)))
