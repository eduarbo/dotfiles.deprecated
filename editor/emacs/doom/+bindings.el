;;; ~/.dotfiles/editor/emacs/doom/+bindings.el -*- lexical-binding: t; -*-
;; This file defines a Spacemacs-esque keybinding scheme

;; expand-region's prompt can't tell what key contract-region is bound to, so we
;; tell it explicitly.
(setq expand-region-contract-fast-key "C-v")

;; Don't let evil-collection interfere with certain keys
(setq evil-collection-key-blacklist
      (list "C-j" "C-k" "gd" "gf" "K" "[" "]" "gz"
            doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))


;;
;; Global keybindings

(map! (:map override
        ;; Make M-x more accessible
        "s-;" #'execute-extended-command
        "s-x" #'execute-extended-command
        "M-x" #'execute-extended-command
        "A-x" #'execute-extended-command

        "s-/" #'which-key-show-top-level
        "s-," #'+nav-flash/blink-cursor
        "s-." #'helpful-key

        "s-[" #'previous-buffer
        "s-]" #'next-buffer

        (:when (featurep! :feature workspaces)
          "s-t" #'+workspace/new
          "s-{" #'+workspace/switch-left
          "s-}" #'+workspace/switch-right)))

;; remaps
(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag

      ;; Smarter newlines, auto-indent on newline
      :i [remap newline] #'newline-and-indent

      (:after vc-annotate
        :map vc-annotate-mode-map
        [remap quit-window] #'kill-this-buffer))

;; top level bindings
(map! :desc "Ex command"          :nv   ";"     #'evil-ex
      :desc "Eval expression"     :nv   ":"     #'eval-expression

      :n  "#"     #'evil-commentary-line
      :v  "#"     #'comment-or-uncomment-region
      :nv [tab]   #'+evil/matchit-or-toggle-fold
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

      (:when (featurep! :feature workspaces)
        :desc "Switch to last workspace" :n  [backtab]  #'+eduarbo/switch-to-last-workspace
        (:map evil-org-mode-map          :n  [backtab]  #'+eduarbo/switch-to-last-workspace))
      :desc "Switch to last buffer"      :n  "~"        #'evil-switch-to-windows-last-buffer
      (:map evil-org-mode-map            :n  "~"        #'evil-switch-to-windows-last-buffer)

      :desc "Resume last completion"     :n  "C-."      (cond ((featurep! :completion ivy)   #'ivy-resume)
                                                              ((featurep! :completion helm)  #'helm-resume))

      ;; Insert mode

      ;; Isearch
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

      ;; workspaces
      (:when (featurep! :feature workspaces)
        :desc "Next workspace"           :n "C-S-h" #'+workspace/switch-left
        :desc "Previous workspace"       :n "C-S-l" #'+workspace/switch-right
        (:leader
          :desc "Switch perspective buffer" :n "SPC"    #'persp-switch-to-buffer
          :desc "Switch buffer"             :n "RET"    #'switch-to-buffer))
      (:unless (featurep! :feature workspaces)
        (:leader
          :desc "Switch buffer"          :n "SPC"    #'switch-to-buffer))

      :desc "Resume last search"    "C-."
      (cond ((featurep! :completion ivy)   #'ivy-resume)
            ((featurep! :completion helm)  #'helm-resume))

     ;; (:when (featurep! :completion company)
     ;;    ;; :after company
     ;;    :i [tab] #'company-complete-common)
     ;;    ;; :i [tab] #'company-indent-or-complete-common)

      ;; Smart tab
      :i [tab] (general-predicate-dispatch nil ; fall back to nearest keymap
                 (and (featurep! :feature snippets)
                      (bound-and-true-p yas-minor-mode)
                      (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                 'yas-expand
                 (and (featurep! :completion company +tng)
                      (+company-has-completion-p))
                 ;; 'company-complete-common)
                 'company-indent-or-complete-common)

      ;; yasnippet
      (:when (featurep! :feature snippets)
        ;; Bind `backtab' to `yas-expand' when snippet expansion available (it will
        ;; still call `company-yasnippet' or `helm-yas-complete' otherwise)
        ;; :i [backtab]
        :i [C-tab]
        (cond ((featurep! :completion company)     #'company-yasnippet)
              ((featurep! :completion helm)        #'helm-yas-complete))

        (:after yasnippet
          (:map yas-minor-mode-map
            :v   [C-tab] #'yas-insert-snippet
            :gi  [C-tab] yas-maybe-expand
            ;; Don't expand snippets with TAB
            :gi [tab] nil))))

;;
;; Module keybinds

(map! :m  "]a"    #'evil-forward-arg
      :m  "[a"    #'evil-backward-arg
      :m  "]o"    #'outline-next-visible-heading
      :m  "[o"    #'outline-previous-visible-heading
      :n  "]b"    #'next-buffer
      :n  "[b"    #'previous-buffer
      :n  "zx"    #'kill-this-buffer
      :n  "ZX"    #'bury-buffer

      (:prefix "g"
        :n  "p"    #'+evil/reselect-paste
        :n  "="    #'widen
        :v  "="    #'+evil:narrow-buffer
        :nv "@"    #'+evil:apply-macro
        :nv "x"    #'evil-exchange
        :v  "p"    #'+evil/paste-preserve-register
        :n  "."    #'call-last-kbd-macro

        :desc "Bookmark current buffer"    :m  "b"    #'bookmark-set
        :desc "Delete bookmark"            :m  "B"    #'bookmark-delete
        :desc "Goto char timer"            :m  "o"    #'avy-goto-char-timer
        :desc "Search in project"          :nv "/"    #'+helm/project-search)

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
        :i  [C-return] #'aya-expand
        :nv [C-return] #'aya-create
        ;; yasnippet
        (:after yasnippet
          (:map yas-keymap
            "C-e"         #'+snippets/goto-end-of-field
            "C-a"         #'+snippets/goto-start-of-field
            [M-right]     #'+snippets/goto-end-of-field
            [M-left]      #'+snippets/goto-start-of-field
            [M-backspace] #'+snippets/delete-to-start-of-field
            [backspace]   #'+snippets/delete-backward-char
            [delete]      #'+snippets/delete-forward-char-or-field)
          (:map yas-minor-mode-map
            :v [tab] #'yas-insert-snippet)))

      (:when (featurep! :feature spellcheck)
        :m "]S" #'flyspell-correct-word-generic
        :m "[S" #'flyspell-correct-previous-word-generic
        (:map flyspell-mouse-map
          "RET"     #'flyspell-correct-word-generic
          [mouse-1] #'flyspell-correct-word-generic))

      (:when (featurep! :feature syntax-checker)
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
        :i "C-SPC"    #'+company/complete
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
            "C-SPC"   #'company-complete-common
            [tab]     #'company-complete-common-or-cycle
            [backtab] #'company-select-previous)
          (:map company-search-map  ; applies to `company-filter-map' too
            "C-n"     #'company-select-next-or-abort
            "C-p"     #'company-select-previous-or-abort
            "C-j"     #'company-select-next-or-abort
            "C-k"     #'company-select-previous-or-abort
            "C-s"     (λ! (company-search-abort) (company-filter-candidates))
            [escape]  #'company-search-abort)
          ;; TAB auto-completion in term buffers
          :map comint-mode-map [tab] #'company-complete))

      (:when (featurep! :completion ivy)
        (:map (help-mode-map helpful-mode-map)
          :n "Q" #'ivy-resume)
        (:after ivy
          :map ivy-minibuffer-map
          "C-SPC" #'ivy-call-and-recenter  ; preview file
          "C-l"   #'ivy-alt-done
          "C-v"   #'yank)
        (:after counsel
          :map counsel-ag-map
          "C-SPC"    #'ivy-call-and-recenter ; preview
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
            [tab]      #'helm-execute-persistent-action
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
        :n "g"         nil
        :n [tab]       #'neotree-quick-look
        :n [return]    #'neotree-enter
        :n [backspace] #'evil-window-prev
        :n "c"         #'neotree-create-node
        :n "r"         #'neotree-rename-node
        :n "d"         #'neotree-delete-node
        :n "j"         #'neotree-next-line
        :n "k"         #'neotree-previous-line
        :n "n"         #'neotree-next-line
        :n "p"         #'neotree-previous-line
        :n "h"         #'+neotree/collapse-or-up
        :n "l"         #'+neotree/expand-or-open
        :n "J"         #'neotree-select-next-sibling-node
        :n "K"         #'neotree-select-previous-sibling-node
        :n "H"         #'neotree-select-up-node
        :n "L"         #'neotree-select-down-node
        :n "G"         #'evil-goto-line
        :n "gg"        #'evil-goto-first-line
        :n "v"         #'neotree-enter-vertical-split
        :n "s"         #'neotree-enter-horizontal-split
        :n "q"         #'neotree-hide
        :n "R"         #'neotree-refresh)

      (:when (featurep! :ui popup)
        :g "C-,"   #'+popup/toggle
        :n "C-~"   #'+popup/raise
        :g "C-x p" #'+popup/other)

      (:when (featurep! :ui vc-gutter)
        :m "]d"    #'git-gutter:next-hunk
        :m "[d"    #'git-gutter:previous-hunk))

;;; :editor
(map! (:when (featurep! :editor format)
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

;;; :tools
(map! (:when (featurep! :tools magit)
        :after evil-magit
        ;; fix conflicts with private bindings
        :map (magit-status-mode-map magit-revision-mode-map)
        "C-j" nil
        "C-k" nil)

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
      :desc "Change the TODO state" :n "SPC" #'org-todo
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

(map! :map emacs-lisp-mode-map
      :nv "K"  #'helpful-at-point)


;;
;; <leader>

(map! :leader
      :desc "M-x"                   ";"    #'execute-extended-command
      :desc "Eval expression"       ":"    #'eval-expression
      :desc "Pop up scratch buffer" "x"    #'doom/open-scratch-buffer
      :desc "Org Capture"           "X"    #'org-capture

      ;; C-u is used by evil
      :desc "Universal argument"    "u"    #'universal-argument
      :desc "Window management"     "w"    #'evil-window-map

      :desc "Find file"             "."    #'find-file
      :desc "Find file in project"  ","    #'projectile-find-file

      :desc "Jump to bookmark"      "j"    #'bookmark-jump

      ;; Prefixed key groups
      (:prefix ("/" . "search")
        :desc "Jump to symbol across buffers" "I" #'imenu-anywhere
        :desc "Search buffer"                 "b" #'swiper
        :desc "Search current directory"      "d"
        (cond ((featurep! :completion helm) #'+helm/project-search-from-cwd)
              ((featurep! :completion ivy)  #'+ivy/project-search-from-cwd))
        :desc "Jump to symbol"                "i" #'imenu
        :desc "Jump to link"                  "l" #'ace-link
        :desc "Look up online"                "o" #'+lookup/online-select
        :desc "Search project"                "p"
        (cond ((featurep! :completion ivy) #'+ivy/project-search)
              ((featurep! :completion helm) #'+helm/project-search)))

      (:prefix ("]" . "next")
        :desc "Increase text size"              "]"  #'text-scale-increase
        :desc "Next Buffer"                     "b"  #'next-buffer
        :desc "Next Diff Hunk"                  "d"  #'git-gutter:next-hunk
        :desc "Next Todo"                       "t"  #'hl-todo-next
        :desc "Next Error"                      "e"  #'next-error
        :desc "Next Workspace"                  "w"  #'+workspace/switch-right
        :desc "Next Spelling error"             "s"  #'evil-next-flyspell-error
        :desc "Next Spelling correction"        "S"  #'flyspell-correct-word-generic)

      (:prefix ("[" . "previous")
        :desc "Decrease text size"              "["  #'text-scale-decrease
        :desc "Previous buffer"                 "b"  #'previous-buffer
        :desc "Previous diff Hunk"              "d"  #'git-gutter:previous-hunk
        :desc "Previous todo"                   "t"  #'hl-todo-previous
        :desc "Previous error"                  "e"  #'previous-error
        :desc "Previous workspace"              "w"  #'+workspace/switch-left
        :desc "Previous spelling error"         "s"  #'evil-prev-flyspell-error
        :desc "Previous spelling correction"    "S"  #'flyspell-correct-previous-word-generic)

      (:when (featurep! :feature workspaces)
        :desc "Switch workspace"            [tab] #'persp-switch

        (:prefix ("l" . "layouts/workspace")
          :desc "Switch workspace"          "TAB" #'persp-switch
          :desc "Display tab bar"           "."   #'+workspace/display
          :desc "New workspace"             "n"   #'+workspace/new
          :desc "Load a past session"       "l"   #'+workspace/load-session
          :desc "Load workspace from file"  "L"   #'+workspace/load
          :desc "Autosave current session"  "s"   #'+workspace/save-session
          :desc "Save workspace to file"    "S"   #'+workspace/save
          :desc "Delete session"            "x"   #'+workspace/kill-session
          :desc "Delete this workspace"     "d"   #'+workspace/delete
          :desc "Rename workspace"          "r"   #'+workspace/rename
          :desc "Restore last session"      "R"   #'+workspace/load-last-session
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
          :desc "Switch perspective buffer" "b" #'persp-switch-to-buffer
          :desc "Switch buffer"             "B" #'switch-to-buffer)
        (:unless (featurep! :feature workspaces)
          :desc "Switch buffer"           "b" #'switch-to-buffer)
        :desc "Kill buffer"                 "k"   #'kill-this-buffer
        :desc "Switch workspace buffer"     "l"   #'helm-projectile-switch-to-buffer
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
        :desc "Open REPL"                   "r"   #'+eval/open-repl
        :desc "Delete trailing whitespace"  "w"   #'delete-trailing-whitespace
        :desc "List errors"                 "x"   #'flycheck-list-errors)

      (:prefix ("f" . "file")
        :desc "Find file"                   "."   #'find-file
        :desc "Find file in project"        "/"   #'projectile-find-file
        :desc "Sudo find file"              ">"   #'doom/sudo-find-file
        :desc "Find file from here"         "?"   #'counsel-file-jump
        :desc "Browse emacs.d"              "E"   #'+default/browse-emacsd
        :desc "Browse private config"       "P"   #'+default/browse-config
        :desc "Recent project files"        "R"   #'projectile-recentf
        :desc "Delete this file"            "X"   #'doom/delete-this-file
        :desc "Find other file"             "a"   #'projectile-find-other-file
        :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
        :desc "Find directory"              "d"   #'dired
        :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
        :desc "Find file in private config" "f"   #'+default/find-in-config
        :desc "Recent files"                "r"   #'recentf-open-files
        :desc "Save file"                   "s"   #'save-buffer
        :desc "Yank filename"               "y"   #'+default/yank-buffer-filename)

      (:prefix ("g" . "git")
        (:when (featurep! :ui vc-gutter)
          :desc "Git revert hunk"             "r"   #'git-gutter:revert-hunk
          :desc "Git stage hunk"              "s"   #'git-gutter:stage-hunk
          :desc "Git time machine"            "t"   #'git-timemachine-toggle
          :desc "Next hunk"                   "]"   #'git-gutter:next-hunk
          :desc "Previous hunk"               "["   #'git-gutter:previous-hunk)
        (:when (featurep! :emacs vc)
          :desc "Browse issues tracker"       "I"   #'+vc/git-browse-issues
          :desc "Browse remote"               "o"   #'+vc/git-browse
          :desc "Git revert file"             "R"   #'vc-revert)
        (:when (featurep! :tools magit)
          :desc "Magit blame"                 "b"   #'magit-blame-addition
          :desc "Magit commit"                "c"   #'magit-commit-create
          :desc "Magit clone"                 "C"   #'+magit/clone
          :desc "Magit diff staged"           "d"   #'magit-diff-buffer-file
          :desc "Magit dispatch"              "D"   #'magit-dispatch-popup
          :desc "Magit find-file"             "f"   #'magit-find-file
          :desc "Magit status"                "g"   #'magit-status
          :desc "Magit file delete"           "x"   #'magit-file-delete
          :desc "MagitHub dispatch"           "h"   #'magithub-dispatch-popup
          :desc "Initialize repo"             "i"   #'magit-init
          :desc "Magit buffer log"            "l"   #'magit-log-buffer-file
          :desc "List repositories"           "L"   #'magit-list-repositories
          :desc "Git stage file"              "S"   #'magit-stage-file
          :desc "Git unstage file"            "U"   #'magit-unstage-file
          :desc "Magit push popup"            "p"   #'magit-push-popup
          :desc "Magit pull popup"            "P"   #'magit-pull-popup
          (:when (featurep! :tools magit +forge)
            :desc "Forge dispatch"               "F"   #'forge-dispatch))
        (:when (featurep! :tools gist)
          :desc "List gists"                  "G"   #'+gist:list))

      (:prefix ("h" . "help")
        :desc "What face"                     "'"   #'doom/what-face
        :desc "Describe at point"             "."   #'helpful-at-point
        :desc "Describe active minor modes"   ";"   #'doom/describe-active-minor-mode
        :desc "Open Doom manual"              "D"   #'doom/open-manual
        :desc "Open vanilla sandbox"          "E"   #'doom/open-vanilla-sandbox
        :desc "Describe face"                 "F"   #'describe-face
        :desc "Find documentation"            "K"   #'+lookup/documentation
        :desc "Command log"                   "L"   #'global-command-log-mode
        :desc "Describe mode"                 "M"   #'describe-mode
        :desc "Reload private config"         "R"   #'doom/reload
        :desc "Print Doom version"            "V"   #'doom/version
        :desc "Apropos"                       "a"   #'apropos
        :desc "Open Bug Report"               "b"   #'doom/open-bug-report
        :desc "Describe char"                 "c"   #'describe-char
        :desc "Describe DOOM module"          "d"   #'doom/describe-module
        :desc "Describe function"             "f"   #'describe-function
        :desc "Emacs help map"                "h"   help-map
        :desc "Info"                          "i"   #'info-lookup-symbol
        :desc "Describe key"                  "k"   #'describe-key
        :desc "Find library"                  "l"   #'find-library
        :desc "View *Messages*"               "m"   #'view-echo-area-messages
        :desc "Toggle profiler"               "p"   #'doom/toggle-profiler
        :desc "Reload theme"                  "r"   #'doom/reload-theme
        :desc "Describe DOOM setting"         "s"   #'doom/describe-setters
        :desc "Describe variable"             "v"   #'describe-variable
        :desc "Man pages"                     "w"   #'+default/man-or-woman)

      (:prefix ("i" . "insert")
        :desc "Insert from clipboard"         "y"   #'+default/yank-pop
        :desc "Insert from evil register"     "r"   #'evil-ex-registers
        :desc "Insert snippet"                "s"   #'yas-insert-snippet)

      (:prefix ("n" . "notes")
        :desc "Start new today's entry"       "d"  #'org-journal-new-entry
        ;; :desc "Open deft"                     "d"  #'deft
        :desc "Open mode notes"               "m"  #'+eduarbo/find-notes-for-major-mode
        :desc "Find file in notes"            "n"  #'+default/find-in-notes
        :desc "Browse notes"                  "N"  #'+default/browse-notes
        :desc "Open project notes"            "p"  #'+eduarbo/find-notes-for-project
        :desc "Org capture"                   "x"  #'org-capture)

      (:prefix ("o" . "open")
        :desc "Org agenda"        "a"  #'org-agenda
        :desc "Default browser"   "b"  #'browse-url-of-file
        :desc "Debugger"          "d"  #'+debug/open
        :desc "REPL"              "r"  #'+eval/open-repl
        :desc "Dired"             "-"  #'dired-jump
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

      (:prefix ("q" . "quit/restart")
        :desc "Quit Emacs"                   "q" #'evil-quit-all
        :desc "Save and quit Emacs"          "Q" #'evil-save-and-quit
        :desc "Quit Emacs & forget session"  "X" #'+workspace/kill-session-and-quit
        :desc "Restart & restore Emacs"      "r" #'+workspace/restart-emacs-then-restore
        :desc "Restart Emacs"                "R" #'restart-emacs)

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
        :desc "Big mode"                     "b" #'doom-big-font-mode
        :desc "Flycheck"                     "f" #'flycheck-mode
        :desc "Evil goggles"                 "g" #'evil-goggles-mode
        :desc "Impatient mode"               "h" #'+impatient-mode/toggle
        :desc "Indent guides"                "i" #'highlight-indentation-mode
        :desc "Indent guides (column)"       "I" #'highlight-indentation-current-column-mode
        :desc "Line numbers"                 "l" #'display-line-numbers-mode
        :desc "Global Line numbers"          "L" #'global-display-line-numbers-mode
        :desc "Flyspell"                     "s" #'flyspell-mode
        :desc "org-tree-slide mode"          "p" #'+org-present/start
        :desc "Visual line mode"             "v" #'visual-line-mode
        :desc "Subword mode"                 "w" #'subword-mode
        :desc "Frame maximized"              "z" #'toggle-frame-maximized
        :desc "Frame fullscreen"             "Z" #'toggle-frame-fullscreen))


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
