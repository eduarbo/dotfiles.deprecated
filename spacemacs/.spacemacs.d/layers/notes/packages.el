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

  (setq org-agenda-files (list notes-directory)
        org-default-notes-file (notes-journal-path))

  (defun my/narrow-and-set-normal ()
    "Narrow to the region and, if in a visual mode, set normal mode."
    (interactive)
    (narrow-to-region (region-beginning) (region-end))
    (if (string= evil-state "visual")
        (progn (evil-normal-state nil)
               (evil-goto-first-line))))

  ;; Taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
  (defun my/narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p)
           (my/narrow-and-set-normal))
          ((and (boundp 'org-src-mode) org-src-mode (not p))
           (org-edit-src-exit))
          ((derived-mode-p 'org-mode)
           (cond ((ignore-errors (org-edit-src-code)))
                 ((ignore-errors (org-narrow-to-block) t))
                 (t (org-narrow-to-subtree))))
          ((derived-mode-p 'latex-mode)
           (LaTeX-narrow-to-environment))
          (t (narrow-to-defun))))

  ;; Remove narrow prefix as `my/narrow-or-widen-dwim` does everything I need in
  ;; one single keystrong
  (unbind-key "n" spacemacs-default-map)
  (spacemacs/set-leader-keys "TAB" 'my/narrow-or-widen-dwim)

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
