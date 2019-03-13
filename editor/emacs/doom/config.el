;;; ~/.dotfiles/editor/emacs/doom/config.el -*- lexical-binding: t; -*-

;;
;; Reasonable defaults

(setq doom-leader-key ","
      doom-localleader-key ", m")

(setq-default
 ;; A E S T H E T I C
 doom-font (font-spec :family "Hack" :size 12)
 doom-big-font (font-spec :family "Hack" :size 20)

 ;; That's me!!!
 user-mail-address "eduarbo@gmail.com"
 user-full-name    "Eduardo Ruiz Macias"

 ;; Enable accents
 ns-alternate-modifier 'none
 ;; Get some context when scrolling
 scroll-margin 10
 ;; default to flyspell prog mode
 flyspell-generic-check-word-predicate #'flyspell-generic-progmode-verify
 ;; use gnu ls to allow dired to sort directories
 insert-directory-program "gls" dired-use-ls-dired t
 ;; Given ~/Projects/FOSS/emacs/lisp/comint.el => emacs/lisp/comint.el
 +doom-modeline-buffer-file-name-style 'relative-from-project
 ;; Call projectile-discover-projects-in-search-path to look for projects in
 ;; list of folders
 projectile-project-search-path '("~/dev")
 ;; A more useful title
 frame-title-format '("%b   —   " (:eval (+workspace-current-name)))

 ;; Protecting me from data loss
 ;; save every 20 characters typed (this is the minimum)
 ;; auto-save-default t
 auto-save-interval 20)

;; Hide line numbers
(remove-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)

(setq +file-templates-alist
      (remove '("\\.org$" :trigger "__" :mode org-mode) +file-templates-alist))

;; FIXME exclude journal notes from templates
;; (setq +file-templates-alist
;;       (cons '("\\(?!/journal/\\).+\\.org$" :trigger "__" :mode org-mode)
;;             (remove '("\\.org$" :trigger "__" :mode org-mode) +file-templates-alist)))

;; Stop in-between "camelCase" words instead of just spaces, hyphens or
;; underscores
(global-subword-mode)

;; whitespace
(defun eduarbo--show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))
(add-hook! (prog-mode conf-mode) #'eduarbo--show-trailing-whitespace)

;; OS specific fixes
(when IS-MAC
  ;; Workaround to enable emoji rendering on macOS
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
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


;;
;; Modules

;; tools/magit
(setq magit-repository-directories '(("~/dev" . 1))
      magit-save-repository-buffers nil)

;; lang/org
(setq +org-capture-todo-file "notes/backlog.org")

(after! org
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

(setq google-translate-default-target-language "es"
      google-translate-default-source-language "en")

(after! highlight-indent-guides
  ;; Hide indent guides
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))

;; completion/helm
;; Show hidden files too
(setq helm-ag-command-option "--hidden"
      +helm-project-search-engines '(rg ag pt))


;; ui/pretty-code
;; enable ligatures only in org-mode
(setq +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))


;;
;; Packages

;; Took from http://blog.binchen.org/posts/how-to-be-extremely-efficient-in-emacs.html
(def-package! keyfreq
  :config
  (setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        forward-char
        backward-char
        previous-line
        next-line
        evil-backward-char
        evil-forward-char
        evil-next-line
        evil-previous-line
        org-self-insert-command
        evil-normal-state
        treemacs-next-line
        treemacs-previous-line))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(after! helm-projectile
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-projectile-recentf-list
                                    helm-source-buffer-not-found)))


(defun eduarbo--set-hl-todo-keyword-faces ()
  (setq hl-todo-keyword-faces `(("TODO"  . ,(face-foreground 'warning))
                                ("FIXME" . ,(face-foreground 'error))
                                ("NOTE"  . ,(face-foreground 'success)))))
;; Overwrite default doom theme faces for todo keywords
(add-hook! 'doom-load-theme-hook #'eduarbo--set-hl-todo-keyword-faces)


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


(after! tide
  ;; Try to ignore case
  (setq completion-ignore-case t
        tide-completion-ignore-case t))


(after! treemacs
  (setq treemacs--icon-size 20))


(after! which-key
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0))
;;
;; Custom

(load! "./+dashboard.el")
(load! "./+bindings.el")
;; (load! "./+bindings.old.el")
