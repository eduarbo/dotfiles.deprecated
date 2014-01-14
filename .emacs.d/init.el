;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

(require 'functions)

;; Set custom theme path
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup packages
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(add-to-list 'el-get-recipe-path (expand-file-name "el-get-recipes" user-emacs-directory))
(setq el-get-user-package-directory (expand-file-name "el-get-init" user-emacs-directory))
(setq el-get-verbose t)

; Declare packages to use
(setq el-get-packages
  '(el-get
    package
    flycheck
    key-chord
    magit
    paredit
    exec-path-from-shell
    diminish
    yasnippet
    auto-complete
    auto-complete-yasnippet
    pretty-mode
    monokai-theme
    molokai-theme
    multiple-cursors
    expand-region
    smartparens
    ace-jump-mode
    undo-tree
    rainbow-delimiters
    rainbow-mode
    smex
    powerline
    drag-stuff
    js2-mode
    skewer-mode
    ))

(el-get 'sync el-get-packages)

;; Set up appearance early
(require 'appearance)

(setq undo-limit 100000)                       ; Increase number of undo

;; Full power!
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq disabled-command-hook nil)               ; Allow all disabled commands

(add-hook 'before-save-hook
          '(lambda ()
             (untabify (point-min) (point-max))
             (delete-trailing-whitespace)))

;; Seed the random-number generator
(random t)

;; Dired jump
(require 'dired-x)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(electric-pair-mode +1)

;; ;; store all backup and autosave files in the tmp dir
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

;; Fullscreen on startup
;(set-frame-parameter nil 'fullscreen 'fullboth)

;; }}}

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Setup extensions
(eval-after-load 'ido '(require 'init-ido))
;; (eval-after-load 'org '(require 'setup-org))
;; (eval-after-load 'dired '(require 'setup-dired))
;; (eval-after-load 'magit '(require 'setup-magit))
;; (eval-after-load 'grep '(require 'setup-rgrep))
;; (eval-after-load 'shell '(require 'setup-shell))
;; (require 'setup-hippie)
;; (require 'setup-yasnippet)
;; (require 'setup-perspective)
;; (require 'setup-ffip)
;; (require 'setup-html-mode)
;; (require 'setup-paredit)

;; ;; Default setup of smartparens
;; (require 'smartparens-config)
;; (setq sp-autoescape-string-quote nil)
;; (--each '(css-mode-hook
;;           restclient-mode-hook
;;           js-mode-hook
;;           ruby-mode
;;           markdown-mode
;;           groovy-mode)
;;   (add-hook it 'turn-on-smartparens-mode))

;; ;; Language specific setup files
;; (eval-after-load 'js2-mode '(require 'setup-js2-mode))
;; (eval-after-load 'ruby-mode '(require 'setup-ruby-mode))
;; (eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
;; (eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; ;; Map files to modes
;; (require 'mode-mappings)

;; (hes-mode)
;; (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; (require 'expand-region)
;; (require 'multiple-cursors)
;; (require 'delsel)
;; (require 'jump-char)
;; (require 'eproject)
;; (require 'wgrep)
;; (require 'smart-forward)
;; (require 'change-inner)
;; (require 'multifiles)

;; Setup key bindings
(require 'key-bindings)

;; ;; misc
;; (require 'project-archetypes)
;; (require 'my-misc)
;; (when is-mac (require 'mac))

;; ;; email, baby
;; (require 'setup-mu4e)

;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
