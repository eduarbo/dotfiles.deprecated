(use-package evil-escape
             :ensure t
             :config
             (setq-default evil-escape-key-sequence "jk")
             :init
             (evil-escape-mode))

(use-package exec-path-from-shell
             :ensure t
             :config
             (exec-path-from-shell-initialize))

(use-package elpy
             :ensure t)

(use-package gruvbox-theme
             :ensure t
             :init
             (load-theme 'gruvbox t))

(use-package wgrep :ensure t)
(use-package wgrep-ag
             :ensure t
             :commands (wgrep-ag-setup))
(autoload 'wgrep-ag-setup "wgrep-ag")

(use-package ag
             :ensure t
             :defer t
             :config
             (add-hook 'ag-mode-hook 'wgrep-ag-setup)
             (setq ag-executable "/usr/local/bin/ag")
             (setq ag-highlight-search t)
             (setq ag-reuse-buffers t)
             (setq ag-reuse-window t))

(use-package exec-path-from-shell
             :ensure t
             :defer t
             :config
             (when (memq window-system '(mac ns))
               (exec-path-from-shell-initialize)))

(use-package helm
             :ensure t
             :defer t
             :diminish helm-mode
             :init
             (setq helm-buffers-fuzzy-matching t)
             :config
             (helm-mode 1)
             (setq helm-autoresize-mode t)
             (setq helm-buffer-max-length 40)
             (bind-key "S-SPC" 'helm-toggle-visible-mark helm-map)
             (bind-key "C-k" 'helm-find-files-up-one-level helm-find-files-map))

(use-package company
             :ensure t
             :defer t
             :init
             (global-company-mode)
             :config
             ;(setq company-tooltip-common-selection ((t (:inherit company-tooltip-selection :background "yellow2" :foreground "#c82829"))))
             ;(setq company-tooltip-selection ((t (:background "yellow2"))))
             (setq company-idle-delay 0.5)
             (setq company-selection-wrap-around t)
             (define-key company-active-map [tab] 'company-complete)
             (define-key company-active-map (kbd "C-n") 'company-select-next)
             (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package swiper
             :ensure t
             :commands swiper
             :bind ("C-s" . swiper))

(use-package dictionary :ensure t)
(use-package emmet-mode :ensure t)
(use-package flycheck :ensure t)
(use-package helm-projectile
             :commands (helm-projectile helm-projectile-switch-project)
             :ensure t)

(use-package markdown-mode :ensure t)

(use-package web-mode
             :ensure t
             :defer t
             :config
             (setq web-mode-attr-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-indent-style 2)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-sql-indent-offset 2))

(use-package yaml-mode :ensure t :defer t)

(use-package yasnippet
             :ensure t
             :defer t
             :config
             (yas-reload-all)
             (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                                      "~/.emacs.d/remote-snippets"))
             (setq tab-always-indent 'complete)
             (setq yas-prompt-functions '(yas-completing-prompt
                                           yas-ido-prompt
                                           yas-dropdown-prompt))
             (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package which-key
             :ensure t
             :diminish ""
             :config
             (which-key-mode t))

(use-package projectile
             :ensure t
             :defer t
             :config
             (projectile-global-mode)
             (setq projectile-enable-caching t))

(use-package highlight-symbol
             :ensure t
             :defer t
             :diminish ""
             :config
             (setq-default highlight-symbol-idle-delay 1.5))

(use-package magit
             :ensure t
             :defer t
             :config
             (setq magit-branch-arguments nil)
             (setq magit-push-always-verify nil)
             (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package mmm-mode
             :ensure t
             :defer t
             :config
             (setq mmm-global-mode 'maybe)
             (mmm-add-classes
               '((markdown-cl
                   :submode emacs-lisp-mode
                   :face mmm-declaration-submode-face
                   :front "^```cl[\n\r]+"
                   :back "^```$")))
             (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-cl))

(provide 'init-packages)
