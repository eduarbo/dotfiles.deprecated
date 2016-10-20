;;; packages.el --- defaults layer packages file for Spacemacs.
;;
;; Copyright (C) 2016  Eduardo Ruiz
;;
;; Author: Eduardo Ruiz <eduarbo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst defaults-packages
  '(avy
    company
    (company-flx :toggle (configuration-layer/package-usedp 'company))
    evil
    evil-args
    flycheck
    gruvbox
    helm
    helm-projectile
    js2-mode
    (sh-script :location built-in)
    spaceline
    subatomic-theme
    web-mode
    undo-tree
    yasnippet))

(defun defaults/post-init-evil ()
  (spacemacs/set-leader-keys
    "TAB" 'evil-switch-to-windows-last-buffer)
  (bind-map-set-keys evil-motion-state-map
    "C-h" 'evil-window-left
    "C-j" 'evil-window-down
    "C-k" 'evil-window-up
    "C-l" 'evil-window-right)
  (bind-map-set-keys evil-normal-state-map
    ":"       'evil-repeat-find-char-reverse
    "<C-tab>" 'evil-jump-item))

(defun defaults/post-init-avy ()
  (bind-map-set-keys evil-normal-state-map
    "s" 'avy-goto-char-timer
    "S" 'avy-goto-char-in-line))

(defun defaults/post-init-evil-args ()
  (bind-map-set-keys evil-normal-state-map
    "L" 'evil-forward-arg
    "H" 'evil-backward-arg)
  (bind-map-set-keys evil-motion-state-map
    "L" 'evil-forward-arg
    "H" 'evil-backward-arg))

(defun defaults/post-init-helm ()
  ;; Enable fuzzy matching for everything
  (setq helm-completion-in-region-fuzzy-match t
        helm-mode-fuzzy-match t))

(defun defaults/post-init-helm-projectile ()
  (spacemacs/set-leader-keys
    "/" 'spacemacs/helm-project-do-grep))

(defun defaults/post-init-web-mode ()
  (setq css-indent-offset tab-width
        web-mode-markup-indent-offset tab-width
        web-mode-css-indent-offset tab-width
        web-mode-code-indent-offset tab-width
        web-mode-attr-indent-offset tab-width))

(defun defaults/post-init-js2-mode ()
  ;; Let flycheck handle parse errors
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)

  ;; Force js to indent 2 levels
  (setq js-indent-level tab-width
        js2-basic-offset tab-width))

(defun defaults/post-init-sh-script ()
  ;; Use same indentation spaces than 'tab-width
  (setq sh-indentation tab-width
        sh-basic-offset tab-width))

(defun defaults/post-init-gruvbox ()
  ;; Darker vertical-border for gruvbox
  (set-face-attribute 'vertical-border nil
                      :foreground "#1d2021" :background nil))

(defun defaults/post-init-subatomic-theme ()
  (set-face-attribute 'vertical-border nil
                      :foreground "#232533")
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 1 :color "#232533"))
  (set-face-attribute 'modeline-inactive nil
                      :box '(:line-width 1 :color "#2a2c3e")))

(defun defaults/post-init-spaceline ()
  (setq powerline-default-separator 'utf-8)
  (custom-set-variables '(powerline-utf-8-separator-left #xe0b0)
                        '(powerline-utf-8-separator-right #xe0b2))
  ;; I need to compile spaceline to take the changes
  (spaceline-compile))

(defun defaults/post-init-undo-tree ()
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo"))))

(defun defaults/post-init-company ()
  (setq
   ;; Get rid of annoying completion-at-point
   tab-always-indent t
   ;; Complete only when I command
   company-idle-delay nil)

  (defun complete-or-insert-tab ()
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common)
      (tab-to-tab-stop)))

  (with-eval-after-load "company"
    (define-key company-active-map (kbd "DEL") 'company-abort)
    (define-key evil-insert-state-map (kbd "TAB") 'complete-or-insert-tab)))

(defun defaults/init-company-flx ()
  (use-package company-flx
    :defer t
    :init
    (setq company-flx-limit 100)
    (with-eval-after-load 'company (company-flx-mode t))))

(defun defaults/post-init-yasnippet ()
  (with-eval-after-load "yasnippet"
    ;; Now I can complete with TAB while snippet expansion is in progress
    (setq yas-keymap
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-n") 'yas-next-field-or-maybe-expand)
            (define-key map (kbd "C-p") 'yas-prev-field)
            (define-key map (kbd "C-g") 'yas-abort-snippet)
            (define-key map (kbd "C-d") 'yas-skip-and-clear-or-delete-char)
            map))))

;; disable jshint, jsonlist, and jscs since I prefer eslint checking
(defun defaults/post-init-flycheck ()
  ;; Use local eslint executable when it's available in node_modules
  ;; Source: http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and
                    root
                    (expand-file-name "node_modules/eslint/bin/eslint.js"
                                      root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  ;; FIXME getting errors when visting js files
  ;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  ;; (add-hook 'js2-mode-hook #'my/use-eslint-from-node-modules)
  ;; (add-hook 'react-mode-hook #'my/use-eslint-from-node-modules)

  (with-eval-after-load 'flycheck
    (setq flycheck-check-syntax-automatically '(mode-enabled save)
          flycheck-disabled-checkers
          (append flycheck-disabled-checkers
                  '(javascript-jshint javascript-jscs json-jsonlist)))))
