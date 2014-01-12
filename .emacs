(require 'cl) ; common lisp goodies, loop

;(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
;;; code:
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
    flycheck
    key-chord
    magit
    powerline
    ;;nyan-mode
    diminish
    yasnippet
    monokai-theme
    molokai-theme
    multiple-cursors
    expand-region
    smartparens
    undo-tree
    rainbow-delimiters
    rainbow-mode
    drag-stuff
    ))

(el-get 'sync el-get-packages)

;; General configurations ------------------------------------------------------------------------------- {{{

(require 'functions)
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA"      . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(load-theme 'molokai t)
;(load-theme 'monokai t)

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

(setq-default indent-tabs-mode nil            ; Use spaces instead of tab
              tab-width 2
              fill-column 80)


(setq scroll-step 1
      scroll-conservatively 50
      scroll-margin 5
      scroll-up-margin 5
      scroll-preserve-screen-position t
      inhibit-startup-message t
      inhibit-splash-screen t
      redisplay-dont-pause t
      color-theme-is-global t
      sentence-end-double-space nil
      mouse-yank-at-point t
      ring-bell-function 'ignore
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      undo-limit 100000                       ; Increase number of undo
      diff-switches "-u")

;; Full power!
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq disabled-command-hook nil)               ; Allow all disabled commands

;; Highlits current line
(global-hl-line-mode t)
(set-face-attribute hl-line-face nil :underline nil)

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

;; Dired jump
(require 'dired-x)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(delete-selection-mode t)

(electric-pair-mode +1)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'ido)
(ido-mode t)

;; Fullscreen on startup
(set-frame-parameter nil 'fullscreen 'fullboth)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; }}}

;; Hooks ------------------------------------------------------------------------------------------------ {{{

;; Whitespace Cleanup
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

(add-hook 'prog-mode-hook 'subword-mode)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; }}}

;; Mappings --------------------------------------------------------------------------------------------- {{{

;; Window controls
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c |") 'toggle-window-split)

(global-set-key (kbd "S-C-h") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-l") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-j") 'shrink-window)
(global-set-key (kbd "S-C-k") 'enlarge-window)

(global-set-key (kbd "C-c d") 'duplicate-line)

(global-set-key (kbd "M-s") 'save-buffer)

(global-set-key (kbd "M-o") 'smart-open-line)
(global-set-key (kbd "M-O") 'smart-open-line-above)

(global-set-key (kbd "C-^") 'top-join-line)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; Shortcut to instant access to .emacs init file
(global-set-key (kbd "C-c E") 'find-user-init-file)

;; Remmaping Kill Whole Line
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)

;; Go Back to Previous Window
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(setq help-char [f1]) ;; I don't want help when I'm just trying to backspace!

(define-key key-translation-map [?\C-h] [?\C-?])

;; {{{

;; TODO ------------------------------------------------------------------------------------------------- {{{

;; - Replace hook to get rid of whitespaces with ethan-wspace
;; - Install Projectile and configure it

;; }}}
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8728727e11f81134904bca7a2bc8af4ead9bd4c866f2fce65686b771bfd5c23d" "d3fe552e196d0599e1b7a22b71f02169ccc2ce7c636d6d941640069fca4f394b" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
