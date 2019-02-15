;;; ~/.dotfiles/editor/emacs/doom/config.el -*- lexical-binding: t; -*-

;;
;; Reasonable defaults

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
 frame-title-format '("%b   —   " (:eval (+workspace-current-name))))

;; Stop in-between "camelCase" words instead of just spaces, hyphens or
;; underscores
(global-subword-mode)

;; whitespace
(defun eduarbo--show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))
(add-hook! (prog-mode conf-mode) #'eduarbo--show-trailing-whitespace)

;; OS specific fixes
(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'window-setup-hook #'toggle-frame-maximized))
  ;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen))


;;
;; Modules

;; tools/magit
(setq magit-repository-directories '(("~/dev" . 1))
      magit-save-repository-buffers nil)

;; lang/org
(setq org-directory (expand-file-name "~/org")
      +org-capture-todo-file "notes/backlog.org"
      org-agenda-files (list org-directory)
      org-ellipsis " ▼ "  ;; ... is boring

      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Personally, markdown #-marks for headlines are more
      ;; elegant.
      org-bullets-bullet-list '("#")

      google-translate-default-target-language "es"
      google-translate-default-source-language "en")

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

(def-package! org-journal
  :after org
  :when (featurep! :lang org)
  :commands (org-journal-new-entry org-journal-search-forever)
  :custom
  (org-journal-dir (expand-file-name "journal" org-directory))
  (org-journal-carryover-items nil)
  ;; Check ~format-time-string~ help for a list of the formatting symbols
  (org-journal-file-format "%Y/%Y-%m-%d %a, %b %e.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-time-prefix "* ")
  ;; (org-journal-time-format "[%F %a %R]")
  (org-journal-hide-entries-p nil))

(after! tide
  ;; Try to ignore case
  (setq completion-ignore-case t
        tide-completion-ignore-case t))


(after! treemacs
  (setq treemacs--icon-size 20))


(after! deft
  (setq deft-directory (expand-file-name "notes" org-directory)))


(after! which-key
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0))
;;
;; Custom

(load! "./+dashboard.el")
(load! "./+bindings.el")
