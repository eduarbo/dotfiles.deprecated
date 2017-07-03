;;; private/eduarbo/config.el -*- lexical-binding: t; -*-

(when (featurep 'evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(defvar +eduarbo-dir
  (file-name-directory load-file-name))

(defvar +eduarbo-snippets-dir
  (expand-file-name "snippets/" +eduarbo-dir))

(setq epa-file-encrypt-to user-mail-address
      auth-sources (list (expand-file-name ".authinfo.gpg" +eduarbo-dir)))

(defun +hlissner*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+hlissner*no-authinfo-for-tramp)


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
                (delete 'yas-installed-snippets-dir
                        yas-snippet-dirs))))

(after! projectile
  ;; Workaround to make the new project name available when hooks are called
  (defun fix-projectile-project-name (projectile-switch-project-by-name &rest args)
    (let* ((projectile-project-name (car args)))
      (apply projectile-switch-project-by-name args)))

  (advice-add #'projectile-switch-project-by-name :around #'fix-projectile-project-name))

;; app/irc
(setq +irc-notifications-watch-strings '("eduarbo"))

(set! :irc "irc.snoonet.org"
  `(:tls t
    :nick "v0"
    :port 6697
    :sasl-username ,(+pass-get-user "irc/snoonet.org")
    :sasl-password ,(+pass-get-secret "irc/snoonet.org")
    :channels (:after-auth "#ynought")))


;; app/email
(setq smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(set! :email "gmail.com"
  '((mu4e-sent-folder       . "/gmail.com/Sent Mail")
    (mu4e-drafts-folder     . "/gmail.com/Drafts")
    (mu4e-trash-folder      . "/gmail.com/Trash")
    (mu4e-refile-folder     . "/gmail.com/All Mail")
    (smtpmail-smtp-user     . "eduarbo")
    (user-mail-address      . "eduarbo@gmail.com")
    (mu4e-compose-signature . "---\nEduardo Ruiz")))

(set! :email "lissner.net"
  '((mu4e-sent-folder       . "/lissner.net/Sent Mail")
    (mu4e-drafts-folder     . "/lissner.net/Drafts")
    (mu4e-trash-folder      . "/lissner.net/Trash")
    (mu4e-refile-folder     . "/lissner.net/All Mail")
    (smtpmail-smtp-user     . "henrik@lissner.net")
    (user-mail-address      . "henrik@lissner.net")
    (mu4e-compose-signature . "---\nHenrik Lissner"))
  t)

;; Enable accents
(setq ns-alternate-modifier 'none)

;; JavaScript
(setq js2-bounce-indent-p t
      ;; Let flycheck handle parse errors
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil)
