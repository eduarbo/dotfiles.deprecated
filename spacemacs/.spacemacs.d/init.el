;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     better-defaults
     themes-megapack
     emacs-lisp
     (auto-completion :variables
                      ;; auto-completion-enable-sort-by-usage t ; It breaks spacemacs
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t)
     (git :variables
          magit-repository-directories '("~/dev/"))
     (version-control :variables
                      version-control-diff-tool 'git-gutter+
                      version-control-global-margin t)
     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell t
            shell-default-position 'bottom
            shell-default-height 30)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t)
     (syntax-checking :variables
                      syntax-checking-enable-tooltips nil)
     deft
     colors
     osx
     erc

     ;; Disabled for security reasons. It exposes my password in plain text!!!
     ;; github

     command-log
     c-c++
     ;; (clojure :variables
     ;;          clojure-enable-fancify-symbols t)
     django
     (elm :variables
          elm-reactor-port "3000"          ; default 8000
          elm-reactor-address "0.0.0.0") ; default 127.0.0.1
     html
     javascript
     lua
     markdown
     org
     pandoc
     (python :variables
             python-enable-yapf-format-on-save t)
     react
     ruby
     shell-scripts
     sql
     vagrant
     yaml

     (crypt :variables
            crypt-gpg-key "eduarbo@gmail.com")
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(company-flx)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(evil-escape
                                    evil-search-highlight-persist
                                    neotree)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 999
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes (if window-system
                           '(subatomic
                             gruvbox)
                         '(subatomic256
                           flatland
                           smyx))
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ";"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header t
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 94
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."

  ;; Enable fuzzy matching for everything
  (setq helm-completion-in-region-fuzzy-match t
        helm-mode-fuzzy-match t)

  ;; keep customize settings in their own file
  (setq custom-file (concat dotspacemacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  (defconst my-notes-directory "~/Dropbox/notes/"
    "Path to my notes directory")

  (defconst my-secrets-path (concat my-notes-directory "secrets.org")
    "Path to my secrets file")

  (defun my-journal-path ()
    "Path to the note of the day"
    (concat my-notes-directory (format-time-string "%Y%m%d.org")))

  (defun open-my-journal ()
    "Open note of the day"
    (interactive)
    (find-file (my-journal-path)))

  (defun open-my-darkest-secrets ()
    "Open my secrets file"
    (interactive)
    (find-file my-secrets-path))

  (defun my-comment-box (b e)
    "Draw a box comment around the region but arrange for the region to extend
to at least the fill column. Place the point after the comment box."
    (interactive "r")
    (let ((e (copy-marker e t)))
      (goto-char b)
      (end-of-line)
      (insert-char ?  (- fill-column (current-column)))
      (comment-box b e 1)
      (goto-char e)
      (set-marker e nil)))

  ;; Use local eslint executable when it's available in node_modules
  ;; Source: http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (file-executable-p eslint)
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  (defun narrow-and-set-normal ()
    "Narrow to the region and, if in a visual mode, set normal mode."
    (interactive)
    (narrow-to-region (region-beginning) (region-end))
    (if (string= evil-state "visual")
        (progn (evil-normal-state nil)
               (evil-goto-first-line))))

  ;; Taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
  (defun narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p)
           (narrow-and-set-normal))
          ((and (boundp 'org-src-mode) org-src-mode (not p))
           (org-edit-src-exit))
          ((derived-mode-p 'org-mode)
           (cond ((ignore-errors (org-edit-src-code)))
                 ((ignore-errors (org-narrow-to-block) t))
                 (t (org-narrow-to-subtree))))
          ((derived-mode-p 'latex-mode)
           (LaTeX-narrow-to-environment))
          (t (narrow-to-defun))))

  ;; Remove narrow prefix as `narrow-or-widen-dwim` does everything I need in
  ;; one single keystrong
  (unbind-key "n" spacemacs-default-map)
  (spacemacs/set-leader-keys "TAB" 'narrow-or-widen-dwim)

  ;; (define-key evil-motion-state-map "]e" 'flycheck-next-error)
  ;; (define-key evil-motion-state-map "[e" 'flycheck-previous-error)

  ;; Swap default bindings
  (spacemacs/set-leader-keys
    "SPC" 'helm-mini
    "."   'spacemacs/alternate-buffer
    ","   'evil-avy-goto-char-2

    ;; I don't need align-repeat, that is why evil-repeat exists
    "xar" 'align-regexp

    ;; mnenonic of Quit Window
    "qw"  'evil-quit

    "cb"  'my-comment-box

    ;; mnenonic of Journal
    "aj" 'open-my-journal
    ;; Secrets
    "aS" 'open-my-darkest-secrets

    "wV"  'split-window-right
    "wv"  'split-window-right-and-focus
    "wS"  'split-window-below
    "ws"  'split-window-below-and-focus)

  (define-key evil-motion-state-map ";" 'evil-ex)
  (define-key evil-evilified-state-map ";" 'evil-ex)
  (define-key evil-ex-map "e" 'helm-find-files)
  (define-key evil-ex-map "b" 'helm-buffers-list)

  (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)

  (bind-map-set-keys evil-normal-state-map "Q" 'fill-paragraph)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Defaults                                                                       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq vc-follow-symlinks nil)

  (setq css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2)

  ;; Force js to indent 2 levels
  (setq-default js2-basic-offset 2
                js-indent-level 2)

  ;; Let flycheck handle parse errors
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)

  (add-to-list 'magic-mode-alist '("import *" . react-mode))

  ;; Support Syntax highlight for my dotfiles
  (add-to-list 'auto-mode-alist (cons "/\\^gitconfig\\'" 'gitconfig-mode))
  (add-to-list 'auto-mode-alist (cons "/\\^gitignore\\'" 'gitignore-mode))
  (add-to-list 'auto-mode-alist (cons "/\\^gitattributes\\'" 'gitattributes-mode))

  ;; Use same indentation spaces than 'tab-width
  (setq sh-indentation tab-width)
  (setq sh-basic-offset tab-width)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Better UI                                                                      ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Darker vertical-border for gruvbox
  (set-face-attribute 'vertical-border nil :foreground "#1d2021" :background nil)

  (setq powerline-default-separator 'utf-8)
  (custom-set-variables '(powerline-utf-8-separator-left #xe0b0)
                        '(powerline-utf-8-separator-right #xe0b2))

  ;; I need to compile spaceline to take the changes
  (spaceline-compile)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Toggles                                                                        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Hide minor mode area
  (spacemacs/toggle-mode-line-minor-modes-off)

  ;; Wrap lines
  ;; Distinguish wrapped lines with curly arrows
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (spacemacs/add-to-hooks 'spacemacs/toggle-auto-fill-mode-on
                          '(org-mode-hook))
  ;; Break lines automatically
  (spacemacs/add-to-hooks 'spacemacs/toggle-visual-line-navigation-on
                          '(org-mode-hook))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Packages                                                                       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun my//include-underscores-in-word-motions ()
    "Include underscores in word motions"
    (modify-syntax-entry ?_ "w")
    )
  (add-hook 'python-mode-hook #'my//include-underscores-in-word-motions)
  (add-hook 'ruby-mode-hook #'my//include-underscores-in-word-motions)
  (add-hook 'js2-mode-hook #'my//include-underscores-in-word-motions)

  ;; Evil-args
  (defun set-lispish-evil-args-delimiters ()
    "Override default args delimiters on lisp languages"
    (setq-local evil-args-delimiters '(" ")))
  (add-hook 'lisp-mode-hook 'set-lispish-evil-args-delimiters)
  (add-hook 'emacs-lisp-mode-hook 'set-lispish-evil-args-delimiters)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args)

  ;; Deft sane defaults
  (with-eval-after-load 'deft
    (setq deft-use-filename-as-title nil
          ;; Disabled until find a way to disable it by buffer. deft-mode-hook
          ;; is fired after timer is created and when deft-mode is called it
          ;; kills all the local variables :/
          ;; TODO contribute with a better implementation
          deft-auto-save-interval 0
          deft-directory my-notes-directory
          deft-use-filter-string-for-filename t
          deft-file-naming-rules '((noslash . "_")
                                   (nospace . "_")
                                   (case-fn . downcase)))
    ;; Do not ask me to follow symlinks
    (define-key deft-mode-map [(shift return)] 'deft-new-file))

  (with-eval-after-load 'org
    (defun journal-file-insert ()
      "Insert's the journal heading based on the file's name."
      (interactive)
      (when (string-match "\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
                          (buffer-name))
        (let ((year  (string-to-number (match-string 1 (buffer-name))))
              (month (string-to-number (match-string 2 (buffer-name))))
              (day   (string-to-number (match-string 3 (buffer-name))))
              (datim nil))
          (setq datim (encode-time 0 0 0 day month year))
          (insert (format-time-string
                   "#+TITLE: Journal - %A, %b %e, %Y\n" datim)
                  "#+PROPERTY: LOGGING lognoterepeat\n\n"))))

    (require 'autoinsert)
    (setq auto-insert-query nil)  ;; don't want to be prompted before insertion
    (add-hook 'find-file-hook 'auto-insert)
    (add-to-list 'auto-insert-alist '(".*/[0-9]*\.org$" . journal-file-insert))

    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline (my-journal-path) "Tasks")
             "* TODO %?\n\n%i\n")
            ("r" "Reminder" entry (file+headline (my-journal-path) "Tasks")
             "* TODO %?\n%^{prompt|SCHEDULED|DEADLINE}: %^t\n\n%i\n")
            ("j" "Journal" entry (file (my-journal-path))
             "* %? :journal:\n%T\n\n%i\n"
             :empty-lines 1)
            ("s" "Secret" entry (file my-secrets-path)
             "* %? :crypt:%^g\n")
            ("l" "Login" entry (file my-secrets-path)
             "* %? :crypt:login:\n %^{username}p\n %^{password}p\n %^{website}\n")))

    (setq org-agenda-files (list my-notes-directory))
    (setq org-default-notes-file (my-journal-path))

    ;; My sensitive data
    (require 'org-crypt)

    ;; Encrypt all entries before saving
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    (setq org-crypt-disable-auto-save 'encrypt)
    (setq org-crypt-key "eduarbo@gmail.com")
    (setq epa-file-encrypt-to org-crypt-key)
    ;; use gpg2 to cache the passphrase with gpg-agent, otherwise it won't work
    (setq epg-gpg-program "gpg2")
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "D" 'org-decrypt-entry)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "E" 'org-encrypt-entries))

  (with-eval-after-load 'company
    ;; Workaround to get rid of annoying completion-at-point in empty strings
    (setq tab-always-indent t)
    (company-flx-mode t))

  ;; disable jshint, jsonlist, and jscs since I prefer eslint checking
  (with-eval-after-load 'flycheck
    (setq flycheck-check-syntax-automatically '(mode-enabled save))
    (setq flycheck-disabled-checkers
          (append flycheck-disabled-checkers
                  '(javascript-jshint javascript-jscs json-jsonlist))))
  )
