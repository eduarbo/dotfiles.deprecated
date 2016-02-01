(setq use-package-always-ensure t)

(use-package elpy)

(use-package wgrep)
(use-package wgrep-ag
  :commands (wgrep-ag-setup))
(autoload 'wgrep-ag-setup "wgrep-ag")

(use-package ag
  :defer t
  :config
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  (setq ag-executable "/usr/local/bin/ag")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

(use-package helm
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

(use-package flycheck
  :commands flycheck-mode
  :init (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package yaml-mode
  :defer t)

(use-package yasnippet
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

(use-package highlight-symbol
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.5))

(provide 'init-packages)
