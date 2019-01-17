;;; ~/.dotfiles/editor/emacs/doom/config.el -*- lexical-binding: t; -*-

;;
;; Reasonable defaults

(setq-default
 ;; A E S T H E T I C
 doom-font (font-spec :family "Hack" :size 12)
 ;; doom-big-font (font-spec :family "Hack" :size 20)
 ;; doom-font (font-spec :family "PragmataPro" :size 13)
 doom-big-font (font-spec :family "PragmataPro" :size 20)

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
 +doom-modeline-buffer-file-name-style 'relative-from-project)


;; whitespace
(defun eduarbo--show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))
(add-hook! (prog-mode conf-mode) #'eduarbo--show-trailing-whitespace)

(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'window-setup-hook #'toggle-frame-maximized))


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
(setq helm-ag-command-option "--hidden"
      ;; FIXME prefer ag over rg as the latter also matches the filenames and
      ;; triggers the search at the first character so it becomes really slow
      ;; when you start typing
      +helm-project-search-engines '(ag rg pt))


;; ui/pretty-code
;; enable ligatures only in org-mode
(setq +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))


;;
;; Packages

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

(load! "+bindings")
