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
 projectile-project-search-path '("~/dev"))

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
  ;; (add-hook 'window-setup-hook #'toggle-frame-maximized))
  (add-hook 'window-setup-hook #'toggle-frame-fullscreen))


;;
;; Modules

;; tools/magit
(setq magit-repository-directories '(("~/dev" . 1))
      magit-save-repository-buffers nil)

;; lang/org
(setq org-directory (expand-file-name "~/Google Drive/org/")
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

(after! helm-projectile
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-projectile-recentf-list
                                    helm-source-buffer-not-found)))

(after! tide
  ;; Try to ignore case
  (setq completion-ignore-case t
        tide-completion-ignore-case t))


(after! treemacs
  (setq treemacs--icon-size 20))


(after! deft
  (setq deft-directory (expand-file-name "notes/" org-directory)))


;;
;; Custom

(load! "./+bindings.el")
