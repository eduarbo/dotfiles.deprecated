;;; private/eduarbo/config.el -*- lexical-binding: t; -*-

(doom/toggle-fullscreen)

(when (featurep 'evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

;; Track last visited workspace when persp-frame-switch is used
(when (featurep 'persp-mode)
  (add-hook 'persp-before-switch-functions '+eduarbo/set-workspace--last))


(defvar +eduarbo-dir
  (file-name-directory load-file-name))

(defvar +eduarbo-snippets-dir
  (expand-file-name "snippets/" +eduarbo-dir))

(setq epa-file-encrypt-to user-mail-address
      auth-sources (list (expand-file-name ".authinfo.gpg" +eduarbo-dir)))


(defun +eduarbo*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+eduarbo*no-authinfo-for-tramp)


(def-package! evil-magit
  :when (and (featurep! :feature evil)
             (featurep! :feature version-control))
  :after magit
  :init (setq evil-magit-want-horizontal-movement t))


(def-package! company-flx
  :when (featurep! :completion company)
  :after company
  :config
  (setq company-flx-limit 2000)
  (with-eval-after-load 'company (company-flx-mode t)))


(after! evil-args
  ;; Set space as a delimiter arguments for lisp-family languages
  (add-hook! 'lisp-mode-hook (setq-local evil-args-delimiters '(" ")))
  (add-hook! 'emacs-lisp-mode-hook (setq-local evil-args-delimiters '(" "))))


(after! evil-mc
  ;; if I'm in insert mode, chances are I want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))


;; Don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs
        (append (list '+eduarbo-snippets-dir)
                (delete 'yas-installed-snippets-dir yas-snippet-dirs))))


(after! projectile
  ;; Workaround to make the new project name available when hooks are called
  (defun fix-projectile-project-name (projectile-switch-project-by-name &rest args)
    (let* ((projectile-project-name (funcall projectile-project-name-function (car args))))
      (apply projectile-switch-project-by-name args)))

  (advice-add #'projectile-switch-project-by-name :around #'fix-projectile-project-name))


(after! magit
  ;; Prevent magit windows to be handled by shackle
  (setq shackle-rules (remove '("^\\*magit" :regexp t :size 0.5 :noesc t :autokill t) shackle-rules))
  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
        magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq magit-repository-directories '("~/dev" "~/Documents/archive/nearsoft/atlassian" "~/.emacs.d" "~/.dotfiles"))
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))


(after! shackle
  ;; Prevent magit windows to be handled by shackle
  (add-to-list 'shackle-rules '("^\\*(?!magit).*" :regexp t :same t))
  (setq shackle-rules (remove '("^\\*"  :regexp t :noselect t :autokill t) shackle-rules)))


(after! company
  (setq completion-ignore-case t
        company-abort-manual-when-too-short t
        company-dabbrev-code-ignore-case t
        ;; Complete only when I command
        company-idle-delay nil))


(after! ivy
  ;; TODO: Use space to narrow results.
  ;; Issue: https://github.com/abo-abo/swiper/issues/360
  (setq ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))


;; ;; app/irc
;; (setq +irc-notifications-watch-strings '("eduarbo"))

;; (set! :irc "irc.snoonet.org"
;;   `(:tls t
;;     :nick "v0"
;;     :port 6697
;;     :sasl-username ,(+pass-get-user "irc/snoonet.org")
;;     :sasl-password ,(+pass-get-secret "irc/snoonet.org")
;;     :channels (:after-auth "#ynought")))


;; ;; app/email
;; (setq smtpmail-stream-type 'starttls
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; (set! :email "gmail.com"
;;   '((mu4e-sent-folder       . "/gmail.com/Sent Mail")
;;     (mu4e-drafts-folder     . "/gmail.com/Drafts")
;;     (mu4e-trash-folder      . "/gmail.com/Trash")
;;     (mu4e-refile-folder     . "/gmail.com/All Mail")
;;     (smtpmail-smtp-user     . "eduarbo")
;;     (user-mail-address      . "eduarbo@gmail.com")
;;     (mu4e-compose-signature . "---\nEduardo Ruiz")))

;; (set! :email "lissner.net"
;;   '((mu4e-sent-folder       . "/lissner.net/Sent Mail")
;;     (mu4e-drafts-folder     . "/lissner.net/Drafts")
;;     (mu4e-trash-folder      . "/lissner.net/Trash")
;;     (mu4e-refile-folder     . "/lissner.net/All Mail")
;;     (smtpmail-smtp-user     . "henrik@lissner.net")
;;     (user-mail-address      . "henrik@lissner.net")
;;     (mu4e-compose-signature . "---\nHenrik Lissner"))
;;   t)
