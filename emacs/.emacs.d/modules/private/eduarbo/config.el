;;; private/eduarbo/config.el -*- lexical-binding: t; -*-

(doom/toggle-fullscreen)

(when (featurep! :feature evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(defvar +eduarbo-dir (file-name-directory load-file-name))
(defvar +eduarbo-snippets-dir (expand-file-name "snippets/" +eduarbo-dir))

(setq epa-file-encrypt-to user-mail-address
      auth-sources (list (expand-file-name ".authinfo.gpg" +eduarbo-dir))
      +doom-modeline-buffer-file-name-style 'relative-from-project)

(defun +eduarbo*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+eduarbo*no-authinfo-for-tramp)


(after! company
  (setq completion-ignore-case t
        company-abort-manual-when-too-short t
        company-dabbrev-code-ignore-case t
        ;; Complete only when I command
        company-idle-delay nil))


(after! counsel
  (setq counsel-ag-base-command "ag --nocolor --nogroup --hidden %s")

  (defun eduarbo-counsel-ag-occur ()
    "Generate a custom occur buffer for `counsel-ag'."
    (counsel-grep-like-occur counsel-ag-base-command))

  ;; be consistent with counsel-ag results
  (ivy-set-occur 'counsel-ag 'eduarbo-counsel-ag-occur))


(after! evil-args
  ;; Set space as a delimiter arguments for lisp-family languages
  (add-hook! 'lisp-mode-hook (setq-local evil-args-delimiters '(" ")))
  (add-hook! 'emacs-lisp-mode-hook (setq-local evil-args-delimiters '(" "))))


(def-package! evil-magit
  :when (and (featurep! :feature evil)
             (featurep! :feature version-control))
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t
        magit-diff-paint-whitespace t))


(after! evil-mc
  ;; if I'm in insert mode, chances are I want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))


(after! flycheck-pos-tip
  ;; make sure flycheck errors take precedence over eldoc. Eldoc delay is 0.5
  (setq flycheck-display-errors-delay 0.6)
  ;; Disable broken tooltip in macOS
  (flycheck-pos-tip-mode -1))


(after! magit
  ;; Prevent magit windows to be handled by shackle
  (setq shackle-rules (remove '("^\\*magit" :regexp t :size 0.5 :noesc t :autokill t) shackle-rules))
  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
        magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq magit-repository-directories '("~/dev" "~/.emacs.d" "~/.dotfiles"))
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(after! shackle
  ;; Prevent magit windows to be handled by shackle
  (add-to-list 'shackle-rules '("^\\*(?!magit).*" :regexp t :same t))
  (setq shackle-rules (remove '("^\\*"  :regexp t :noselect t :autokill t) shackle-rules)))


(after! projectile
  ;; TODO: Remove after this workaround is merged into master
  ;; https://github.com/bbatsov/projectile/issues/1165
  (defun projectile-discover-projects-in-directory (directory)
  "Discover any projects in DIRECTORY and add them to the projectile cache.
This function is not recursive and only adds projects with roots
at the top level of DIRECTORY."
  (interactive
   (list (read-directory-name "Starting directory: ")))
  (let ((subdirs (directory-files directory t)))
    (mapcar
     (lambda (dir)
       (when (and (file-directory-p dir)
                  (not (member (file-name-nondirectory dir) '(".." "."))))
         (let ((default-directory dir)
               (projectile-cached-project-root dir))
           (when (projectile-project-p)
             (projectile-add-known-project (projectile-project-root))))))
     subdirs))))


(after! smartparens
  ;; Auto-close more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))


;; Don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs
        (append (list '+eduarbo-snippets-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))
