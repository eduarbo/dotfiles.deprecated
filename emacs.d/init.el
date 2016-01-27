(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                  (concat user-emacs-directory "backups")))))
;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;;; File type overrides.
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(require 'init-essentials)
(require 'init-utils)
(require 'init-platform)
(require 'init-elpa)
(require 'init-appearance)

(maybe-require-package 'use-package)
(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'init-org)
(require 'init-fonts)
(require 'init-gtags)
(require 'init-evil)
(require 'init-maps)
(require 'init-w3m)
(require 'init-powerline)
(require 'init-flycheck)
(require 'init-tmux)
(require 'init-packages)
(require 'init-hooks)
(require 'init-linum)

;; emacs server
(require 'server)
(unless (server-running-p) (server-start))

(provide 'init)
;;; init.el ends here
