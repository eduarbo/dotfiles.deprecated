;;; ~/.dotfiles/editor/emacs/doom/config.el -*- lexical-binding: t; -*-

;;
;; Reasonable defaults

(setq doom-leader-key ","
      doom-localleader-key ", m")

;; That's me!!!
(setq user-mail-address "eduarbo@gmail.com"
      user-full-name    "Eduardo Ruiz Macias"
      epa-file-encrypt-to user-mail-address)

;; A E S T H E T I C
(setq doom-font (font-spec :family "Hack" :size 14)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14))

(setq google-translate-default-target-language "es"
      google-translate-default-source-language "en")

(setq which-key-idle-delay 0.3
      which-key-idle-secondary-delay 0)

;; Call projectile-discover-projects-in-search-path to look for projects in
;; list of folders
(setq projectile-project-search-path '("~/dev" "~/dev/opentable"))

(setq-default
 ;; Enable accents
 ns-alternate-modifier 'none
 ;; Get some context when scrolling
 scroll-margin 10
 ;; use gnu ls to allow dired to sort directories
 insert-directory-program "gls" dired-use-ls-dired t
 ;; A more useful title
 frame-title-format '("%b   —   " (:eval (+workspace-current-name)))

 ;; Protecting me from data loss. Save every 20 chars typed (this is the minimum)
 auto-save-visited-interval 20)

(setq +file-templates-alist
      (remove '("\\.org$" :trigger "__" :mode org-mode) +file-templates-alist))

;; FIXME exclude journal notes from templates
;; (setq +file-templates-alist
;;       (cons '("\\(?!/journal/\\).+\\.org$" :trigger "__" :mode org-mode)
;;             (remove '("\\.org$" :trigger "__" :mode org-mode) +file-templates-alist)))


;; Hide line numbers
(remove-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)
;; Hide indent lines
(remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode)

(defun eduarbo--set-hl-todo-keyword-faces ()
  (setq hl-todo-keyword-faces `(("TODO"  . ,(face-foreground 'warning))
                                ("FIXME" . ,(face-foreground 'error))
                                ("NOTE"  . ,(face-foreground 'success)))))
;; Overwrite default doom theme faces for todo keywords
(add-hook! 'doom-load-theme-hook #'eduarbo--set-hl-todo-keyword-faces)

;; whitespace
(defun eduarbo--show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))
(add-hook! (prog-mode conf-mode) #'eduarbo--show-trailing-whitespace)

;; OS specific fixes
(when IS-MAC
  ;; Use the OS X Emoji font for Emoticons
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend)
  ;; Workaround to enable emoji rendering on macOS
  ;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'window-setup-hook #'toggle-frame-maximized))
  ;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen))

;; Syntax highlighting for systemd Files
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-mode))

;; Stop in-between "camelCase" words instead of just spaces, hyphens or
;; underscores
(global-subword-mode)


;;
;; Modules

;; lang/org
(setq +org-capture-todo-file "notes/backlog.org")

(after! org
  (add-to-list 'org-modules 'org-habit t)
  (setq org-hide-emphasis-markers t
        org-directory (expand-file-name "~/org")
        ;; TODO Use `org-directory` instead of the hardcoded path
        org-agenda-files '("~/org/notes" "~/org/journal")
        org-ellipsis " ▼ "  ;; ˅ ⌄ ↓ ⤵ ▼ ↴ ⬎ ⤷

        org-todo-keywords
        '((sequence "[ ](i)" "[-](p)" "[?](m)" "|" "[X](x)")
          (sequence "TODO(t)" "DOING(D)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
          (sequence "LATER(l)" "MAYBE(m)" "SOMEDAY(s)" "IDEA(i)" "|" "CANCELLED(c)"))

        org-agenda-custom-commands
        '(("g" . "GTD contexts")
          ;; ("gh" "Home" tags-todo "HOME")
          ("gl" "Later" tags-todo "LATER")
          ("G" "GTD Block Agenda"
           ((todo "STARTED")
            (todo "DOING")
            (todo "NEXT"))
           ((org-agenda-prefix-format "[ ] %T: ")
            (org-agenda-with-colors nil)
            (org-agenda-compact-blocks t)
            (org-agenda-remove-tags t)
            (ps-number-of-columns 2)
            (ps-landscape-mode t))
           ;;nil                      ;; i.e., no local settings
           ))

        org-todo-keyword-faces
        '(("[-]" :inherit font-lock-constant-face :weight bold)
          ("[?]" :inherit warning :weight bold)
          ("TODO" :inherit error :weight bold)
          ("DOING" :inherit warning :weight bold)
          ("NEXT" :inherit success :weight bold)
          ("WAITING" :inherit default :weight bold)
          ("TODAY" :foreground "#dd8844" :weight bold)
          ("LATER" :foreground "#44b9b1" :weight bold)
          ("IDEA" :foreground "#5699AF" :weight bold)
          ("MAYBE" :foreground "#5699AF" :weight bold)
          ("SOMEDAY" :foreground "#5699AF" :weight bold))

        ;; TODO Setup tags: http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html

        ;; The standard unicode characters are usually misaligned depending on
        ;; the font. This bugs me. Personally, markdown #-marks for headlines
        ;; are more elegant.
        org-bullets-bullet-list '("#")))

(def-package! org-journal
  :when (featurep! :lang org)
  :after org
  :commands (org-journal-new-entry org-journal-search-forever)
  :custom
  (org-journal-dir (expand-file-name "journal" org-directory))
  (org-journal-carryover-items nil)
  ;; Check ~format-time-string~ help for a list of the formatting symbols
  (org-extend-today-until 4) ;; sometimes my days end at 4am
  (org-journal-file-format "%Y/%Y-%m-%d %A.org")
  (org-journal-date-prefix "#+TITLE: ")
  ;; FIXME Exclude journals from doom file-templates, that is overriding the TITLE
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-time-prefix "* ")
  ;; (org-journal-time-format "[%F %a %R]")
  (org-journal-hide-entries-p nil))


;; completion/company
(after! company
  ;; On-demand code completion. I don't often need it
  (setq company-idle-delay nil))


;; completion/helm
(after! helm
  ;; Show hidden files too
  (setq helm-ag-command-option "--hidden"))

(after! helm-projectile
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-projectile-recentf-list
                                    helm-source-buffer-not-found)))


;; ui/modeline
(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        ;; Given ~/Projects/FOSS/emacs/lisp/comint.el => emacs/lisp/comint.el
        ;; doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Given ~/Projects/FOSS/emacs/lisp/comint.el => ~/Projects/FOSS/emacs/l/comint.el
        doom-modeline-buffer-file-name-style 'truncate-upto-project))


;; ui/treemacs
(after! treemacs
  (setq treemacs--icon-size 20))

;; Enable custom treemacs theme (all-the-icons must be installed!)
(doom-themes-treemacs-config)


;; tools/flyspell
(after! flyspell
  ;; default to flyspell prog mode
  (setq flyspell-generic-check-word-predicate #'flyspell-generic-progmode-verify))


;; tools/magit
(setq magit-repository-directories '(("~/dev" . 2))
      magit-save-repository-buffers nil)


;; tools/lsp
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil))

(after! lsp-ui
  ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
  ;; disable it by default
  (setq lsp-ui-sideline-enable nil))


;; lang/javascript
(after! tide
  (setq tide-always-show-documentation nil
        tide-completion-detailed nil)
  ;; Try to ignore case
  (setq completion-ignore-case t
        tide-completion-ignore-case t))


;;
;; Custom


(load! "./+dashboard.el")
(load! "./+bindings.el")
