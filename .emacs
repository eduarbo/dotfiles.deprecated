(require 'cl) ; common lisp goodies, loop

;(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(add-to-list 'load-path "~/.emacs.d/init")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes") ; My own recipes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq el-get-user-package-directory "~/.emacs.d/el-get-init")
(setq el-get-verbose t)

; Declare packages to use
(setq el-get-packages
  '(el-get
    package
    nyan-mode
    expand-region
    smartparens
    ))

(el-get 'sync el-get-packages)

;; General configurations

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA"      . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(load-theme 'spolsky t)

;; UI settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq inhibit-splash-screen t)
(setq redisplay-dont-pause  t)

(setq scroll-step 1
      scroll-conservatively 50
      scroll-margin 5
      scroll-up-margin 5
      scroll-preserve-screen-position t)

(setq inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

;; Highlits current line
(global-hl-line-mode)

;; No ring or visual warnings
(setq ring-bell-function 'ignore)

;; For camelCase parts selection
(global-subword-mode 1)

;; UTF-8
(setq locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(add-hook 'before-save-hook
          '(lambda ()
             (untabify (point-min) (point-max))
             (delete-trailing-whitespace)))

(defalias 'yes-or-no-p 'y-or-n-p)

;; Seed the random-number generator
(random t)

(setq fill-column)
