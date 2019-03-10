;;; ~/.dotfiles/editor/emacs/doom/+bindings2.el -*- lexical-binding: t; -*-

;; This file defines a Spacemacs-esque keybinding scheme

;; Don't let evil-collection interfere with certain keys
(setq evil-collection-key-blacklist
      (list "C-j" "C-k" "gd" "gf" "K" "[" "]" "gz"
            doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))


;;
;; Global keybindings

(map! (:map override
        ;; A little sandbox to run code in
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      "M-x" #'execute-extended-command
      "A-x" #'execute-extended-command

      "s-;" #'execute-extended-command
      "s-x" #'execute-extended-command
      "s-/" #'which-key-show-top-level
      "s-," #'+nav-flash/blink-cursor
      "s-." #'helpful-key

      "s-[" #'previous-buffer
      "s-]" #'next-buffer

      (:when (featurep! :feature workspaces)
        "s-t" #'+workspace/new
        "s-{" #'+workspace/switch-left
        "s-}" #'+workspace/switch-right)

      [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag

      ;; Smart tab
      :i [tab] (general-predicate-dispatch nil ; fall back to nearest keymap
                 (and (featurep! :feature snippets)
                      (bound-and-true-p yas-minor-mode)
                      (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                 'yas-expand
                 (and (featurep! :completion company +tng)
                      (+company-has-completion-p))
                 '+company/complete)
      :n [tab] (general-predicate-dispatch nil
                 (and (featurep! :editor fold)
                      (save-excursion (end-of-line) (invisible-p (point))))
                 '+fold/toggle
                 (fboundp 'evilmi-jump-items)
                 'evilmi-jump-items)
      :v [tab] (general-predicate-dispatch nil
                 (and (bound-and-true-p yas-minor-mode)
                      (or (eq evil-visual-selection 'line)
                          (and (fboundp 'evilmi-jump-items)
                               (save-excursion
                                 (/= (point)
                                     (progn (evilmi-jump-items nil)
                                            (point)))))))
                 'yas-insert-snippet
                 (fboundp 'evilmi-jump-items)
                 'evilmi-jump-items)

      ;; Smarter RET in normal mode
      :n "RET" (general-predicate-dispatch nil
                 (and (bound-and-true-p flyspell-mode)
                      (+flyspell-correction-at-point-p))
                 'flyspell-correct-word-generic)

      ;; Smarter newlines
      :i [remap newline] #'newline-and-indent  ; auto-indent on newline
      :i "C-j"           #'+default/newline    ; default behavior

      (:after vc-annotate
        :map vc-annotate-mode-map
        [remap quit-window] #'kill-this-buffer)

      ;; misc
      :nv ";"     #'evil-ex
      :nv ":"     #'eval-expression
      :n  "#"     #'evil-commentary-line
      :v  "#"     #'comment-or-uncomment-region
      :v  "@"     #'+evil:apply-macro

      ;; Shift text
      :n  "<"     #'evil-shift-left-line
      :n  ">"     #'evil-shift-right-line
      ;; don't leave visual mode after shifting
      :v  "<"     #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"     #'+evil/visual-indent  ; vnoremap > >gv

      ;; FIXME: Ensure they really move to previous/next buffer
      :n  "H"     #'previous-buffer
      :n  "L"     #'next-buffer

      :n  "C-."   (cond ((featurep! :completion ivy)   #'ivy-resume)
                        ((featurep! :completion helm)  #'helm-resume))

      :n  "~"        #'evil-switch-to-windows-last-buffer
      (:map evil-org-mode-map
        :n  "~"        #'evil-switch-to-windows-last-buffer)

      ;; Insert mode
      :gi "C-s"          #'isearch-forward
      (:map isearch-mode-map
        :gi "C-S-s"      #'isearch-repeat-backward)

      ;; Behave like a backspace
      :gi [C-backspace]  #'backward-delete-char-untabify

      ;; :gi [S-backspace]  #'delete-forward-char

      :gi "C-d"          #'evil-delete-line
      :gi "C-S-d"        #'evil-delete-whole-line
      :gi "C-S-u"        #'evil-change-whole-line
      :gi "C-S-w"        #'backward-kill-sexp

      :gi "C-S-a"        #'sp-beginning-of-sexp
      :gi "C-S-e"        #'sp-end-of-sexp

      :gi "C-S-f"        #'sp-forward-sexp
      :gi "C-S-b"        #'sp-backward-sexp

      :gi "C-h"          #'left-char
      :gi "C-l"          #'right-char
      :gi "C-S-h"        #'sp-backward-symbol
      :gi "C-S-l"        #'sp-forward-symbol

      ;; Basic editing
      :gi "S-SPC"        #'tab-to-tab-stop
      ;; TODO: Tranpose last two WORDS not those around
      :gi "C-t"          #'transpose-words
      ;; TODO: Tranpose last two SEXPS not those around
      :gi "C-S-t"        #'transpose-sexps
      ;; Smarter C-a/C-e for both Emacs and Evil. C-a will jump to indentation.
      ;; Pressing it again will send you to the true bol. Same goes for C-e, except
      ;; it will ignore comments+trailing whitespace before jumping to eol.
      :gi "C-a"   #'doom/backward-to-bol-or-indent
      :gi "C-e"   #'doom/forward-to-last-non-comment-or-eol

      :nv "C-a"   #'evil-numbers/inc-at-pt
      :nv "C-S-a" #'evil-numbers/dec-at-pt

      ;; Easier window/tab navigation
      :en "C-h"   #'evil-window-left
      :en "C-j"   #'evil-window-down
      :en "C-k"   #'evil-window-up
      :en "C-l"   #'evil-window-right

      ;; expand-region
      :v "v"   (general-predicate-dispatch 'er/expand-region
                 (eq (evil-visual-type) 'line)
                 'evil-visual-char)
      :v "C-v" #'er/contract-region

      ;; Global evil keybinds
      :m  "]a"    #'evil-forward-arg
      :m  "[a"    #'evil-backward-arg
      :m  "]o"    #'outline-next-visible-heading
      :m  "[o"    #'outline-previous-visible-heading
      :n  "]b"    #'next-buffer
      :n  "[b"    #'previous-buffer
      :n  "zx"    #'kill-this-buffer
      :n  "ZX"    #'bury-buffer
      :n  "gp"    #'+evil/reselect-paste
      :n  "g="    #'widen
      :v  "g="    #'+evil:narrow-buffer
      :nv "g@"    #'+evil:apply-macro
      :nv "gc"    #'evil-commentary
      :nv "gx"    #'evil-exchange
      :n  "gb"    #'bookmark-set
      :n  "gB"    #'bookmark-delete
      :nv "go"    #'avy-goto-char-timer
      :nv "g/"    #'+helm/project-search
      :nv "C-a"   #'evil-numbers/inc-at-pt
      :nv "C-S-a" #'evil-numbers/dec-at-pt
      :v  "gp"    #'+evil/paste-preserve-register
      :v  "@"     #'+evil:apply-macro
      ;; repeat in visual mode (FIXME buggy)
      :v  "."     #'+evil:apply-macro
      :n  "g."     #'call-last-kbd-macro
      ;; don't leave visual mode after shifting
      :v  "<"     #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"     #'+evil/visual-indent  ; vnoremap > >gv

      ;; window management (prefix "C-w")
      (:map evil-window-map
        ;; Navigation
        "C-h"     #'evil-window-left
        "C-j"     #'evil-window-down
        "C-k"     #'evil-window-up
        "C-l"     #'evil-window-right
        "C-w"     #'other-window
        ;; Swapping windows
        "H"       #'+evil/window-move-left
        "J"       #'+evil/window-move-down
        "K"       #'+evil/window-move-up
        "L"       #'+evil/window-move-right
        "C-S-w"   #'ace-swap-window
        ;; Window undo/redo
        "u"       #'winner-undo
        "C-u"     #'winner-undo
        "C-r"     #'winner-redo
        "o"       #'doom/window-enlargen
        "O"       #'doom/window-zoom
        ;; Delete window
        "c"       #'+workspace/close-window-or-workspace
        "C-C"     #'ace-delete-window)

      ;; Plugins
      ;; evil-easymotion
      :m  "gs"    #'+evil/easymotion  ; lazy-load `evil-easymotion'
      (:after evil-easymotion
        :map evilem-map
        "a" (evilem-create #'evil-forward-arg)
        "A" (evilem-create #'evil-backward-arg)
        "s" (evilem-create #'evil-snipe-repeat
                           :name 'evil-easymotion-snipe-forward
                           :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                           :bind ((evil-snipe-scope 'buffer)
                                  (evil-snipe-enable-highlight)
                                  (evil-snipe-enable-incremental-highlight)))
        "S" (evilem-create #'evil-snipe-repeat
                           :name 'evil-easymotion-snipe-backward
                           :pre-hook (save-excursion (call-interactively #'evil-snipe-S))
                           :bind ((evil-snipe-scope 'buffer)
                                  (evil-snipe-enable-highlight)
                                  (evil-snipe-enable-incremental-highlight)))
        "SPC" #'avy-goto-char-timer
        "/" (evilem-create #'evil-ex-search-next
                           :pre-hook (save-excursion (call-interactively #'evil-ex-search-forward))
                           :bind ((evil-search-wrap)))
        "?" (evilem-create #'evil-ex-search-previous
                           :pre-hook (save-excursion (call-interactively #'evil-ex-search-backward))
                           :bind ((evil-search-wrap))))

      ;; text object plugins
      :textobj "x" #'evil-inner-xml-attr               #'evil-outer-xml-attr
      :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
      :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
      :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
      :textobj "k" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
      :textobj "j" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down

      ;; evil-snipe
      (:after evil-snipe
        :map evil-snipe-parent-transient-map
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively
                   (evilem-create #'evil-snipe-repeat
                                  :bind ((evil-snipe-scope 'whole-buffer)
                                         (evil-snipe-enable-highlight)
                                         (evil-snipe-enable-incremental-highlight))))))

      ;; evil-surround
      :v "S" #'evil-surround-region
      :o "s" #'evil-surround-edit
      :o "S" #'evil-Surround-edit)

;; help
(map! (:map help-map
        "'"   #'doom/what-face
        "."   #'helpful-at-point ; replaces `display-local-help'
        "a"   #'apropos ; replaces `apropos-command'
        "A"   #'doom/describe-autodefs
        "B"   #'doom/open-bug-report
        "c"   #'describe-char
        "d"   #'doom/describe-module ; replaces `apropos-documentation' b/c `apropos' covers this
        "D"   #'doom/open-manual
        "E"   #'doom/open-vanilla-sandbox
        "F"   #'describe-face ; replaces `Info-got-emacs-command-node' b/c redundant w/ helpful
        "f"   #'describe-function
        "h"   #'+lookup/documentation ; replaces `view-hello-file' b/c annoying
        "H"   #'view-echo-area-messages
        "i"   #'info-lookup-symbol
        "k"   #'describe-key
        "l"   #'find-library ; replaces `describe-package' b/c redundant w/ `doom/describe-package'
        "L"   #'global-command-log-mode ; replaces `describe-language-environment' b/c remapped to C-l
        "C-l" #'describe-language-environment
        "M"   #'describe-mode
        "m"   #'doom/describe-active-minor-mode
        "C-m" #'info-emacs-manual
        "n"   #'doom/open-news ; replaces `view-emacs-news' b/c it's on C-n too
        "O"   #'+lookup/online
        "p"   #'doom/describe-package ; replaces `finder-by-keyword'
        "P"   #'doom/toggle-profiler ; replaces `help-with-tutorial' b/c not useful for evil users
        "r" nil ; replaces `info-emacs-manual' b/c it's on C-m now
        "s"   #'doom/describe-setters
        "v"   #'describe-variable
        (:prefix ("r" . "reload")
          "r"   #'doom/reload
          "t"   #'doom/reload-theme
          "p"   #'doom/reload-packages
          "f"   #'doom/reload-font
          "P"   #'doom/reload-project)
        "V"   #'doom/version ; replaces `finder-by-keyword'
        "W"   #'+default/man-or-woman))


;;
;; Module keybinds

;;; :feature
(map! (:when (featurep! :feature debugger)
        :after realgud
        :map realgud:shortkey-mode-map
        :n "j" #'evil-next-line
        :n "k" #'evil-previous-line
        :n "h" #'evil-backward-char
        :n "l" #'evil-forward-char
        :n "c" #'realgud:cmd-continue
        :m "n" #'realgud:cmd-next
        :m "b" #'realgud:cmd-break
        :m "B" #'realgud:cmd-clear)

      (:when (featurep! :feature eval)
        :g  "M-r" #'+eval/buffer
        :nv "gr"  #'+eval:region
        :n  "gR"  #'+eval/buffer
        :v  "gR"  #'+eval:replace-region)

      (:when (featurep! :feature lookup)
        :nv "K"  #'+lookup/documentation
        :nv "gd" #'+lookup/definition
        :nv "gD" #'+lookup/references
        :nv "gf" #'+lookup/file)

      (:when (featurep! :feature snippets)
        ;; auto-yasnippet
        :i  [C-tab] #'aya-expand
        :nv [C-tab] #'aya-create
        ;; yasnippet
        (:after yasnippet
          (:map yas-keymap
            "C-e"         #'+snippets/goto-end-of-field
            "C-a"         #'+snippets/goto-start-of-field
            [M-right]     #'+snippets/goto-end-of-field
            [M-left]      #'+snippets/goto-start-of-field
            [M-backspace] #'+snippets/delete-to-start-of-field
            [backspace]   #'+snippets/delete-backward-char
            [delete]      #'+snippets/delete-forward-char-or-field)))

      (:when (featurep! :tools flyspell)
        :m "]s" #'evil-next-flyspell-error
        :m "[s" #'evil-prev-flyspell-error
        :m "]S" #'flyspell-correct-word-generic
        :m "[S" #'flyspell-correct-previous-word-generic
        (:map flyspell-mouse-map
          "RET"     #'flyspell-correct-word-generic
          [mouse-1] #'flyspell-correct-word-generic))

      (:when (featurep! :tools flycheck)
        :m "]e" #'next-error
        :m "[e" #'previous-error
        (:after flycheck
          :map flycheck-error-list-mode-map
          :n "C-n" #'flycheck-error-list-next-error
          :n "C-p" #'flycheck-error-list-previous-error
          :n "j"   #'flycheck-error-list-next-error
          :n "k"   #'flycheck-error-list-previous-error
          :n "RET" #'flycheck-error-list-goto-error))

      (:when (featurep! :feature workspaces)
        :n  "C-`"  #'+eduarbo/switch-to-last-workspace
        (:map evil-org-mode-map
          :n  "C-`"  #'+eduarbo/switch-to-last-workspace)

        :n "C-S-l" #'+workspace/switch-right
        :n "C-S-h" #'+workspace/switch-left
        :n "gt"    #'+workspace/switch-right
        :n "gT"    #'+workspace/switch-left
        :n "]w"    #'+workspace/switch-right
        :n "[w"    #'+workspace/switch-left
        :g "M-1"   (λ! (+workspace/switch-to 0))
        :g "M-2"   (λ! (+workspace/switch-to 1))
        :g "M-3"   (λ! (+workspace/switch-to 2))
        :g "M-4"   (λ! (+workspace/switch-to 3))
        :g "M-5"   (λ! (+workspace/switch-to 4))
        :g "M-6"   (λ! (+workspace/switch-to 5))
        :g "M-7"   (λ! (+workspace/switch-to 6))
        :g "M-8"   (λ! (+workspace/switch-to 7))
        :g "M-9"   (λ! (+workspace/switch-to 8))
        :g "M-0"   #'+workspace/switch-to-last
        :g "M-t"   #'+workspace/new
        :g "M-T"   #'+workspace/display))

;;; :completion
(map! (:when (featurep! :completion company)
        :i "C-@"      #'+company/complete
        :i "C-RET"    #'+company/complete
        (:prefix "C-x"
          :i "C-l"    #'+company/whole-lines
          :i "C-k"    #'+company/dict-or-keywords
          :i "C-f"    #'company-files
          :i "C-]"    #'company-etags
          :i "s"      #'company-ispell
          :i "C-s"    #'company-yasnippet
          :i "C-o"    #'company-capf
          :i "C-n"    #'+company/dabbrev
          :i "C-p"    #'+company/dabbrev-code-previous)
        (:after company
          (:map company-active-map
            "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
            "C-n"     #'company-select-next
            "C-p"     #'company-select-previous
            "C-j"     #'company-select-next
            "C-k"     #'company-select-previous
            "C-h"     #'company-show-doc-buffer
            "C-u"     #'company-previous-page
            "C-d"     #'company-next-page
            "C-s"     #'company-filter-candidates
            "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
                            ((featurep! :completion ivy)  #'counsel-company))
            "C-RET"   #'company-complete-common
            "TAB"     #'company-complete-common-or-cycle
            [backtab] #'company-select-previous)
          (:map company-search-map  ; applies to `company-filter-map' too
            "C-n"     #'company-select-next-or-abort
            "C-p"     #'company-select-previous-or-abort
            "C-j"     #'company-select-next-or-abort
            "C-k"     #'company-select-previous-or-abort
            "C-s"     (λ! (company-search-abort) (company-filter-candidates))
            "ESC"     #'company-search-abort)
          ;; TAB auto-completion in term buffers
          :map comint-mode-map "TAB" #'company-complete))

      (:when (featurep! :completion ivy)
        (:map (help-mode-map helpful-mode-map)
          :n "Q" #'ivy-resume)
        (:after ivy
          :map ivy-minibuffer-map
          "C-RET" #'ivy-call-and-recenter  ; preview file
          "C-l"   #'ivy-alt-done
          "C-v"   #'yank)
        (:after counsel
          :map counsel-ag-map
          "C-RET"    #'ivy-call-and-recenter ; preview
          "C-l"      #'ivy-done
          [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
          [C-return] (+ivy-do-action! #'+ivy-git-grep-other-window-action))
        (:after swiper
          :map swiper-map
          [backtab] #'+ivy/wgrep-occur))

      (:when (featurep! :completion helm)
        (:after helm
          (:map helm-map
            [left]     #'left-char
            [right]    #'right-char
            "C-S-n"    #'helm-next-source
            "C-S-p"    #'helm-previous-source
            "C-j"      #'helm-next-line
            "C-k"      #'helm-previous-line
            "C-S-j"    #'helm-next-source
            "C-S-k"    #'helm-previous-source
            "C-f"      #'helm-next-page
            "C-S-f"    #'helm-previous-page
            "C-u"      #'helm-delete-minibuffer-contents
            "C-w"      #'backward-kill-word
            "C-r"      #'evil-paste-from-register ; Evil registers in helm! Glorious!
            "C-s"      #'helm-minibuffer-history
            "C-b"      #'backward-word
            ;; Swap TAB and C-z
            "TAB"      #'helm-execute-persistent-action
            "C-z"      #'helm-select-action)
          (:after swiper-helm
            :map swiper-helm-keymap [backtab] #'helm-ag-edit)
          (:after helm-ag
            :map helm-ag-map
            "C--"      #'+helm-do-ag-decrease-context
            "C-="      #'+helm-do-ag-increase-context
            [backtab]  #'helm-ag-edit
            [left]     nil
            [right]    nil)
          (:after helm-files
            :map (helm-find-files-map helm-read-file-map)
            [C-return] #'helm-ff-run-switch-other-window
            "C-w"      #'helm-find-files-up-one-level)
          (:after helm-locate
            :map helm-generic-files-map
            [C-return] #'helm-ff-run-switch-other-window)
          (:after helm-buffers
            :map helm-buffer-map
            [C-return] #'helm-buffer-switch-other-window)
          (:after helm-regexp
            :map helm-moccur-map
            [C-return] #'helm-moccur-run-goto-line-ow)
          (:after helm-grep
            :map helm-grep-map
            [C-return] #'helm-grep-run-other-window-action))))

;;; :ui
(map! (:when (featurep! :ui hl-todo)
        :m "]t" #'hl-todo-next
        :m "[t" #'hl-todo-previous)

      (:when (featurep! :ui neotree)
        :after neotree
        :map neotree-mode-map
        :n "g"     nil
        :n "TAB"   #'neotree-quick-look
        :n "RET"   #'neotree-enter
        :n "DEL"   #'evil-window-prev
        :n "c"     #'neotree-create-node
        :n "r"     #'neotree-rename-node
        :n "d"     #'neotree-delete-node
        :n "j"     #'neotree-next-line
        :n "k"     #'neotree-previous-line
        :n "n"     #'neotree-next-line
        :n "p"     #'neotree-previous-line
        :n "h"     #'+neotree/collapse-or-up
        :n "l"     #'+neotree/expand-or-open
        :n "J"     #'neotree-select-next-sibling-node
        :n "K"     #'neotree-select-previous-sibling-node
        :n "H"     #'neotree-select-up-node
        :n "L"     #'neotree-select-down-node
        :n "G"     #'evil-goto-line
        :n "gg"    #'evil-goto-first-line
        :n "v"     #'neotree-enter-vertical-split
        :n "s"     #'neotree-enter-horizontal-split
        :n "q"     #'neotree-hide
        :n "R"     #'neotree-refresh)

      (:when (featurep! :ui popup)
        :n "C-,"   #'+popup/toggle
        :n "C-~"   #'+popup/raise
        :g "C-x p" #'+popup/other)

      (:when (featurep! :ui vc-gutter)
        :m "]d"    #'git-gutter:next-hunk
        :m "[d"    #'git-gutter:previous-hunk))

;;; :editor
(map! (:when (featurep! :editor fold)
        :nv [(shift return)] #'+fold/toggle)

      (:when (featurep! :editor format)
        :n "gQ"    #'+format:region)

      (:when (featurep! :editor multiple-cursors)
        ;; evil-mc
        (:prefix "gz"
          :nv "d" #'evil-mc-make-and-goto-next-match
          :nv "D" #'evil-mc-make-and-goto-prev-match
          :nv "j" #'evil-mc-make-cursor-move-next-line
          :nv "k" #'evil-mc-make-cursor-move-prev-line
          :nv "m" #'evil-mc-make-all-cursors
          :nv "n" #'evil-mc-make-and-goto-next-cursor
          :nv "N" #'evil-mc-make-and-goto-last-cursor
          :nv "p" #'evil-mc-make-and-goto-prev-cursor
          :nv "P" #'evil-mc-make-and-goto-first-cursor
          :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
          :nv "u" #'evil-mc-undo-all-cursors
          :nv "z" #'+multiple-cursors/evil-mc-make-cursor-here)
        (:after evil-mc
          :map evil-mc-key-map
          :nv "C-n" #'evil-mc-make-and-goto-next-cursor
          :nv "C-N" #'evil-mc-make-and-goto-last-cursor
          :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
          :nv "C-P" #'evil-mc-make-and-goto-first-cursor)
        ;; evil-multiedit
        :v  "R"     #'evil-multiedit-match-all
        :n  "M-d"   #'evil-multiedit-match-symbol-and-next
        :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
        :v  "M-d"   #'evil-multiedit-match-and-next
        :v  "M-D"   #'evil-multiedit-match-and-prev
        :nv "C-M-d" #'evil-multiedit-restore
        (:after evil-multiedit
          (:map evil-multiedit-state-map
            "M-d" #'evil-multiedit-match-and-next
            "M-D" #'evil-multiedit-match-and-prev
            "RET" #'evil-multiedit-toggle-or-restrict-region)
          (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
            "C-n" #'evil-multiedit-next
            "C-p" #'evil-multiedit-prev)))

      (:when (featurep! :editor rotate-text)
        :n "!" #'rotate-text))

;;; :emacs
(map! (:when (featurep! :emacs vc)
        :after git-timemachine
        :map git-timemachine-mode-map
        :n "C-p" #'git-timemachine-show-previous-revision
        :n "C-n" #'git-timemachine-show-next-revision
        :n "[["  #'git-timemachine-show-previous-revision
        :n "]]"  #'git-timemachine-show-next-revision
        :n "q"   #'git-timemachine-quit
        :n "gb"  #'git-timemachine-blame))
(map! :map emacs-lisp-mode-map
      :nv "K"  #'helpful-at-point)

;;; :tools
(map! (:when (featurep! :tools magit)
        (:after evil-magit
          ;; fix conflicts with private bindings
          :map (magit-status-mode-map magit-revision-mode-map)
          "C-j" nil
          "C-k" nil)
        (:map transient-map
          "q" #'transient-quit-one))

      (:when (featurep! :tools gist)
        :after gist
        :map gist-list-menu-mode-map
        :n "RET" #'+gist/open-current
        :n "b"   #'gist-browse-current-url
        :n "c"   #'gist-add-buffer
        :n "d"   #'gist-kill-current
        :n "f"   #'gist-fork
        :n "q"   #'quit-window
        :n "r"   #'gist-list-reload
        :n "s"   #'gist-star
        :n "S"   #'gist-unstar
        :n "y"   #'gist-print-current-url))

;;; :lang
(map! (:when (featurep! :lang markdown)
        :after markdown-mode
        :map markdown-mode-map
        ;; fix conflicts with private bindings
        [backspace] nil))

(map! :map org-mode-map
      :n "SPC" #'org-todo
      (:when IS-MAC
        :ni "s-o"   (λ! (+org/insert-item 'below))
        :ni "s-O"   (λ! (+org/insert-item 'above))))

;; A fresh start
(map! :map org-mode-map
      :after org
      :localleader
      ;; Unmap the whole map
      "" nil)
(map! :map org-mode-map
      :after org
      :localleader

      :desc "Schedule"              :n  "s"   #'org-schedule
      :desc "Set deadline"          :n  "d"   #'org-deadline
      :desc "Set tags"              :n  "t"   #'org-set-tags-command
      ;; Basic char syntax:
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html#Basic-Char-Syntax
      :desc "Bold"                  :v  "b"   (λ! (org-emphasize ?*)) ;; bold
      :desc "Italic"                :v  "i"   (λ! (org-emphasize ?/)) ;; italic
      :desc "Insert link"           :v  "k"   #'org-insert-link
      :desc "Remove link"           :nv "K"   #'+org/remove-link
      :desc "Store link to heading" :n  "l"   #'org-store-link
      :desc "Monospace/code"        :v  "m"   (λ! (org-emphasize ?~)) ;; monospace/code
      :desc "Restore format"        :v  "r"   (λ! (org-emphasize ?\s)) ;; restore format
      :desc "Underline"             :v  "u"   (λ! (org-emphasize ?_)) ;; underline
      :desc "Verbose"               :v  "v"   (λ! (org-emphasize ?=)) ;; verbose
      :desc "Strikethrough"         :v  "x"   (λ! (org-emphasize ?+)) ;; strikethrough

      (:prefix ("c" . "clock/timer")
        :desc "Start timer"                   :n "c" #'org-clock-in
        :desc "Stop timer"                    :n "C" #'org-clock-out
        :desc "Display total time on heading" :n "d" #'org-clock-display
        :desc "Create table report"           :n "d" #'org-clock-report
        :desc "Go to running timer's entry"   :n "g" #'org-clock-goto
        :desc "Select past timers entry"      :n "G" (λ! (org-clock-goto 'select))
        :desc "Cancel running timer"          :n "x" #'org-clock-cancel))

(map! :mode org-journal-mode
      :localleader
      :map org-journal-mode-map
      "n" #'org-journal-open-next-entry
      "p" #'org-journal-open-previous-entry)


;;
;; <leader>

(map! :leader
      :desc "Eval expression"       ":"    #'eval-expression
      :desc "M-x"                   ";"    #'execute-extended-command
      :desc "Pop up scratch buffer" "x"    #'doom/open-scratch-buffer
      :desc "Org Capture"           "X"    #'org-capture

      ;; C-u is used by evil
      :desc "Universal argument"    "u"    #'universal-argument
      :desc "window"                "w"    evil-window-map
      :desc "help"                  "h"    help-map

      :desc "Toggle last popup"     "~"    #'+popup/toggle
      :desc "Find file"             "."    #'find-file

      (:when (featurep! :feature workspaces)
        :desc "Switch workspace buffer" "," #'persp-switch-to-buffer
        :desc "Switch buffer"           "<" #'switch-to-buffer)
      (:unless (featurep! :feature workspaces)
        :desc "Switch buffer"           "," #'switch-to-buffer)

      :desc "Resume last search"    "'"
      (cond ((featurep! :completion ivy)   #'ivy-resume)
            ((featurep! :completion helm)  #'helm-resume))

      :desc "Find file in project"  "SPC"  #'projectile-find-file
      :desc "Blink cursor line"     "DEL"  #'+default/yank-pop
      :desc "Jump to bookmark"      "RET"  #'bookmark-jump

      ;; Prefixed key groups
      (:prefix ("/" . "search")
        :desc "Jump to symbol across buffers" "I" #'imenu-anywhere
        :desc "Search buffer"                 "b" #'swiper
        :desc "Search current directory"      "d" #'+default/search-from-cwd
        :desc "Jump to symbol"                "i" #'imenu
        :desc "Jump to link"                  "l" #'ace-link
        :desc "Look up online"                "o" #'+lookup/online-select
        :desc "Search project"                "p" #'+default/search-project)

      (:when (featurep! :feature workspaces)
        :desc "Switch workspace"            "TAB" #'persp-switch

        (:prefix ("l" . "workspace")
          :desc "Display tab bar"           "TAB" #'+workspace/display
          :desc "New workspace"             "n"   #'+workspace/new
          :desc "Load a past session"       "l"   #'+workspace/load-session
          :desc "Load workspace from file"  "L"   #'+workspace/load
          :desc "Autosave current session"  "s"   #'+workspace/save-session
          :desc "Save workspace to file"    "S"   #'+workspace/save
          :desc "Switch workspace"          "."   #'+workspace/switch-to
          :desc "Delete session"            "x"   #'+workspace/kill-session
          :desc "Delete this workspace"     "d"   #'+workspace/delete
          :desc "Rename workspace"          "r"   #'+workspace/rename
          :desc "Restore last session"      "R"   #'+workspace/restore-last-session
          :desc "Next workspace"            "]"   #'+workspace/switch-right
          :desc "Previous workspace"        "["   #'+workspace/switch-left
          :desc "Switch to 1st workspace"   "1"   (λ! (+workspace/switch-to 0))
          :desc "Switch to 2nd workspace"   "2"   (λ! (+workspace/switch-to 1))
          :desc "Switch to 3rd workspace"   "3"   (λ! (+workspace/switch-to 2))
          :desc "Switch to 4th workspace"   "4"   (λ! (+workspace/switch-to 3))
          :desc "Switch to 5th workspace"   "5"   (λ! (+workspace/switch-to 4))
          :desc "Switch to 6th workspace"   "6"   (λ! (+workspace/switch-to 5))
          :desc "Switch to 7th workspace"   "7"   (λ! (+workspace/switch-to 6))
          :desc "Switch to 8th workspace"   "8"   (λ! (+workspace/switch-to 7))
          :desc "Switch to 9th workspace"   "9"   (λ! (+workspace/switch-to 8))
          :desc "Switch to last workspace"  "0"   #'+workspace/switch-to-last))

      (:prefix ("b" . "buffer")
        :desc "Toggle narrowing"            "-"   #'doom/clone-and-narrow-buffer
        :desc "New empty buffer"            "N"   #'evil-buffer-new
        :desc "Sudo edit this file"         "S"   #'doom/sudo-this-file
        :desc "Previous buffer"             "["   #'previous-buffer
        :desc "Next buffer"                 "]"   #'next-buffer
        (:when (featurep! :feature workspaces)
          :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
          :desc "Switch buffer"           "B" #'switch-to-buffer)
        (:unless (featurep! :feature workspaces)
          :desc "Switch buffer"           "b" #'switch-to-buffer)
        :desc "Kill buffer"                 "k"   #'kill-this-buffer
        :desc "Next buffer"                 "n"   #'next-buffer
        :desc "Kill other buffers"          "o"   #'doom/kill-other-buffers
        :desc "Previous buffer"             "p"   #'previous-buffer
        :desc "Save buffer"                 "s"   #'save-buffer
        :desc "Pop scratch buffer"          "x"   #'doom/open-scratch-buffer
        :desc "Bury buffer"                 "z"   #'bury-buffer)

      (:prefix ("c" . "code")
        :desc "Jump to references"          "D"   #'+lookup/references
        :desc "Evaluate & replace region"   "E"   #'+eval:replace-region
        :desc "Delete trailing newlines"    "W"   #'doom/delete-trailing-newlines
        :desc "Build tasks"                 "b"   #'+eval/build
        :desc "Jump to definition"          "d"   #'+lookup/definition
        :desc "Evaluate buffer/region"      "e"   #'+eval/buffer-or-region
        :desc "Format buffer/region"        "f"   #'+format/region-or-buffer
        :desc "Open REPL"                   "r"   #'+eval/open-repl-other-window
        :desc "Delete trailing whitespace"  "w"   #'delete-trailing-whitespace
        :desc "List errors"                 "x"   #'flycheck-list-errors)

      (:prefix ("f" . "file")
        :desc "Find file from here"         "."   (if (fboundp 'counsel-file-jump) #'counsel-file-jump #'find-file)
        :desc "Find file in other project"  ">"   #'doom/browse-in-other-project
        :desc "Find file in project"        "/"   #'projectile-find-file
        :desc "Find file in other project"  "?"   #'doom/find-file-in-other-project
        :desc "Browse emacs.d"              "E"   #'+default/browse-emacsd
        :desc "Browse private config"       "P"   #'doom/open-private-config
        :desc "Recent project files"        "R"   #'projectile-recentf
        :desc "Delete this file"            "X"   #'doom/delete-this-file
        :desc "Find other file"             "a"   #'projectile-find-other-file
        :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
        :desc "Find directory"              "d"   #'dired
        :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
        :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
        :desc "Recent files"                "r"   #'recentf-open-files
        :desc "Save file"                   "s"   #'save-buffer
        :desc "Sudo find file"              "S"   #'doom/sudo-find-file
        :desc "Yank filename"               "y"   #'+default/yank-buffer-filename)

      (:prefix ("g" . "git")
        :desc "Git revert file"             "R"   #'vc-revert
        :desc "Git link for line or region" "y"   #'git-link
        (:when (featurep! :ui vc-gutter)
          :desc "Git revert hunk"           "r"   #'git-gutter:revert-hunk
          :desc "Git stage hunk"            "s"   #'git-gutter:stage-hunk
          :desc "Git time machine"          "t"   #'git-timemachine-toggle
          :desc "Jump to next hunk"         "]"   #'git-gutter:next-hunk
          :desc "Jump to previous hunk"     "["   #'git-gutter:previous-hunk)
        (:when (featurep! :tools magit)
          :desc "Magit dispatch"            "/"   #'magit-dispatch
          :desc "Magit diff staged"         "d"   #'magit-diff-buffer-file
          :desc "Forge dispatch"            "'"   #'forge-dispatch
          :desc "Magit status"              "g"   #'magit-status
          :desc "Magit file delete"         "x"   #'magit-file-delete
          :desc "Magit blame"               "b"   #'magit-blame-addition
          :desc "Commit"                    "c"   #'magit-commit-create
          :desc "Clone repo"                "C"   #'+magit/clone
          :desc "Magit fetch"               "F"   #'magit-fetch
          :desc "Issue"                     "I"   #'forge-create-issue
          :desc "Magit buffer log"          "L"   #'magit-log
          :desc "Pull request"              "P"   #'forge-create-pullreq
          :desc "Initialize repo"           "i"   #'magit-init
          :desc "Git stage file"            "S"   #'magit-stage-file
          :desc "Git unstage file"          "U"   #'magit-unstage-file
          (:prefix ("f" . "find")
            :desc "Find file"                 "f"   #'magit-find-file
            :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
            :desc "Find commit"               "c"   #'magit-show-commit
            :desc "Find issue"                "i"   #'forge-visit-issue
            :desc "Find pull request"         "p"   #'forge-visit-pullreq)
          (:prefix ("o" . "open in browser")
            :desc "Browse region or line"     "."   #'+vc/git-browse-region-or-line
            :desc "Browse remote"             "r"   #'forge-browse-remote
            :desc "Browse commit"             "c"   #'forge-browse-commit
            :desc "Browse an issue"           "i"   #'forge-browse-issue
            :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
            :desc "Browse issues"             "I"   #'forge-browse-issues
            :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
          (:prefix ("l" . "list")
            (:when (featurep! :tools gist)
              :desc "List gists"              "g"   #'+gist:list)
            :desc "List repositories"         "r"   #'magit-list-repositories
            :desc "List submodules"           "s"   #'magit-list-submodules
            :desc "List issues"               "i"   #'forge-list-issues
            :desc "List pull requests"        "p"   #'forge-list-pullreqs
            :desc "List notifications"        "n"   #'forge-list-notifications)))

      (:prefix ("i" . "insert")
        :desc "Insert from clipboard"         "y"   #'+default/yank-pop
        :desc "Insert from evil register"     "r"   #'evil-ex-registers
        :desc "Insert snippet"                "s"   #'yas-insert-snippet)

      (:prefix ("n" . "notes")
        :desc "New Journal entry"   "d"  #'org-journal-new-entry
        :desc "Open mode notes"     "m"  #'+eduarbo/find-notes-for-major-mode
        :desc "Find file in notes"  "n"  #'+default/find-in-notes
        :desc "Browse notes"        "N"  #'+default/browse-notes
        :desc "Open project notes"  "p"  #'+eduarbo/find-notes-for-project
        :desc "Org capture"         "x"  #'org-capture)

      (:prefix ("o" . "open")
        :desc "Org agenda"         "a"  #'org-agenda
        :desc "Default browser"    "b"  #'browse-url-of-file
        :desc "Debugger"           "d"  #'+debug/open
        :desc "REPL"               "r"  #'+eval/open-repl-other-window
        :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
        :desc "Dired"              "-"  #'dired-jump
        (:when (featurep! :ui neotree)
          :desc "Project sidebar"              "p" #'+neotree/open
          :desc "Find file in project sidebar" "P" #'+neotree/find-this-file)
        (:when (featurep! :ui treemacs)
          :desc "Project sidebar" "p" #'+treemacs/toggle
          :desc "Find file in project sidebar" "P" #'+treemacs/find-file)
        (:when (featurep! :emacs imenu)
          :desc "Imenu sidebar" "i" #'imenu-list-smart-toggle)
        (:when (featurep! :emacs term)
          :desc "Terminal"          "t" #'+term/open
          :desc "Terminal in popup" "T" #'+term/open-popup-in-project)
        (:when (featurep! :tools vterm)
          :desc "Terminal"          "t" #'+vterm/open
          :desc "Terminal in popup" "T" #'+vterm/open-popup-in-project)
        (:when (featurep! :emacs eshell)
          :desc "Eshell"            "e" #'+eshell/open
          :desc "Eshell in popup"   "E" #'+eshell/open-popup)
        (:when (featurep! :collab floobits)
          (:prefix ("f" . "floobits")
            "c" #'floobits-clear-highlights
            "f" #'floobits-follow-user
            "j" #'floobits-join-workspace
            "l" #'floobits-leave-workspace
            "R" #'floobits-share-dir-private
            "s" #'floobits-summon
            "t" #'floobits-follow-mode-toggle
            "U" #'floobits-share-dir-public))
        (:when (featurep! :tools macos)
          :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
          :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
          :desc "Send to Transmit"           "u" #'+macos/send-to-transmit
          :desc "Send project to Transmit"   "U" #'+macos/send-project-to-transmit
          :desc "Send to Launchbar"          "l" #'+macos/send-to-launchbar
          :desc "Send project to Launchbar"  "L" #'+macos/send-project-to-launchbar)
        (:when (featurep! :tools docker)
          :desc "Docker" "D" #'docker))

      (:prefix ("p" . "project")
        :desc "Browse project"               "." #'+default/browse-project
        :desc "Find file in project"         "/" #'projectile-find-file
        :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
        :desc "Compile project"              "c" #'projectile-compile-project
        :desc "Discover projects"            "d" #'projectile-discover-projects-in-search-path
        :desc "Find other file"              "o" #'projectile-find-other-file
        :desc "Switch project"               "p" #'projectile-switch-project
        :desc "Recent project files"         "r" #'projectile-recentf
        :desc "List project tasks"           "t" #'+default/project-tasks
        :desc "Invalidate cache"             "x" #'projectile-invalidate-cache)

      (:prefix ("q" . "session")
        :desc "Quit Emacs"                   "q" #'evil-quit-all
        :desc "Save and quit Emacs"          "Q" #'evil-save-and-quit
        :desc "Quick save current session"   "s" #'doom/quicksave-session
        :desc "Restore last session"         "l" #'doom/quickload-session
        :desc "Save session to file"         "S" #'doom/save-session
        :desc "Restore session from file"    "L" #'doom/load-session
        :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
        :desc "Restart Emacs"                "R" #'doom/restart
        (:when (featurep! :feature workspaces)
          :desc "Quit Emacs & forget session"  "X" #'+workspace/kill-session-and-quit))

      (:when (featurep! :tools upload)
        (:prefix ("r" . "remote")
          :desc "Upload local"               "u" #'ssh-deploy-upload-handler
          :desc "Upload local (force)"       "U" #'ssh-deploy-upload-handler-forced
          :desc "Download remote"            "d" #'ssh-deploy-download-handler
          :desc "Diff local & remote"        "D" #'ssh-deploy-diff-handler
          :desc "Browse remote files"        "." #'ssh-deploy-browse-remote-handler
          :desc "Detect remote changes"      ">" #'ssh-deploy-remote-changes-handler))

      (:when (featurep! :feature snippets)
        (:prefix ("s" . "snippets")
          :desc "New snippet"                "n" #'yas-new-snippet
          :desc "Insert snippet"             "i" #'yas-insert-snippet
          :desc "Jump to mode snippet"       "/" #'yas-visit-snippet-file
          :desc "Jump to snippet"            "s" #'+snippets/find-file
          :desc "Browse snippets"            "S" #'+snippets/browse
          :desc "Reload snippets"            "r" #'yas-reload-all))

      (:prefix ("t" . "toggle")
        :desc "Flyspell"                     "s" #'flyspell-mode
        :desc "Flycheck"                     "f" #'flycheck-mode
        :desc "Line numbers"                 "l" #'display-line-numbers-mode
        :desc "Global Line numbers"          "L" #'global-display-line-numbers-mode
        :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
        :desc "Indent guides"                "i" #'highlight-indentation-mode
        :desc "Indent guides (column)"       "I" #'highlight-indentation-current-column-mode
        :desc "Impatient mode"               "h" #'+impatient-mode/toggle
        :desc "Big mode"                     "b" #'doom-big-font-mode
        :desc "Evil goggles"                 "g" #'evil-goggles-mode
        :desc "org-tree-slide mode"          "p" #'+org-present/start
        :desc "Visual line mode"             "v" #'visual-line-mode
        :desc "Subword mode"                 "w" #'subword-mode
        :desc "Frame maximized"              "Z" #'toggle-frame-maximized))


;;
;; Universal motion repeating keys

(defvar +default-repeat-keys (cons ";" ",")
  "The keys to use for repeating motions.

This is a cons cell whose CAR is the key for repeating a motion forward, and
whose CDR is for repeating backward. They should both be kbd-able strings.")

(when +default-repeat-keys
  (defmacro do-repeat! (command next-func prev-func)
    "Makes ; and , the universal repeat-keys in evil-mode. These keys can be
customized by changing `+default-repeat-forward-key' and
`+default-repeat-backward-key'."
    (let ((fn-sym (intern (format "+default*repeat-%s" (doom-unquote command)))))
      `(progn
         (defun ,fn-sym (&rest _)
           (define-key! :states 'motion
             (car +default-repeat-keys) #',next-func
             (cdr +default-repeat-keys) #',prev-func))
         (advice-add #',command :before #',fn-sym))))

  ;; n/N
  (do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

  ;; f/F/t/T/s/S
  (setq evil-snipe-repeat-keys nil
        evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;
  (do-repeat! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse)

  ;; */#
  (do-repeat! evil-visualstar/begin-search-forward
              evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-visualstar/begin-search-backward
              evil-ex-search-previous evil-ex-search-next))


;;
;; Universal evil integration

(when (featurep! :feature evil +everywhere)
  ;; Have C-u behave similarly to `doom/backward-to-bol-or-indent'.
  ;; NOTE SPC u replaces C-u as the universal argument.
  (map! :gi "C-u" #'doom/backward-kill-to-bol-and-indent
        :gi "C-w" #'backward-kill-word
        ;; Vimmish ex motion keys
        :gi "C-b" #'backward-word
        :gi "C-f" #'forward-word)

  (after! view
    (define-key view-mode-map [escape] #'View-quit-all))
  (after! man
    (evil-define-key* 'normal Man-mode-map "q" #'kill-this-buffer))

  ;; Minibuffer
  (define-key! evil-ex-completion-map
    "C-a" #'move-beginning-of-line
    "C-b" #'backward-word
    "C-s" (if (featurep! :completion ivy)
              #'counsel-minibuffer-history
            #'helm-minibuffer-history))

  (define-key! :keymaps +default-minibuffer-maps
    [escape] #'abort-recursive-edit
    "C-v"    #'yank
    "C-z"    (λ! (ignore-errors (call-interactively #'undo)))
    "C-a"    #'move-beginning-of-line
    "C-b"    #'backward-word
    "C-r"    #'evil-paste-from-register
    ;; Scrolling lines
    "C-j"    #'next-line
    "C-k"    #'previous-line
    "C-S-j"  #'scroll-up-command
    "C-S-k"  #'scroll-down-command))
