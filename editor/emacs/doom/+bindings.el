;;; ~/.dotfiles/editor/emacs/doom/+bindings.el -*- lexical-binding: t; -*-

(map!
 :gnvime "M-;"         #'execute-extended-command
 :gnvime "M-/"         #'helpful-key

 :n      "RET"         (if (featurep! :completion company)
                           #'persp-switch-to-buffer
                        #'switch-to-buffer)

 :n      "SPC"         (when (featurep! :completion helm)
                         #'+eduarbo/omni-helm-mini)

 ;; navigate between buffers & workspaces
 :n      "H"           #'previous-buffer
 :n      "L"           #'next-buffer
 ;; all terrain version
 :gnvime "M-["         #'previous-buffer
 :gnvime "M-]"         #'next-buffer

 (:when (featurep! :ui popup)
   :gnvime "C-`"       #'+popup/toggle)

 ;; Easier window/tab navigation
 :en     "C-h"         #'evil-window-left
 :en     "C-j"         #'evil-window-down
 :en     "C-k"         #'evil-window-up
 :en     "C-l"         #'evil-window-right

 ;; go to next/prev shorcuts like in other apps
 :gnvime "C-<"         #'+workspace/switch-left
 :gnvime "C->"         #'+workspace/switch-right
 ;; all terrain version
 :gnvime "M-{"         #'+workspace/switch-left
 :gnvime "M-}"         #'+workspace/switch-right

 :nv     "#"           #'evil-commentary-line
 :nv     ";"           #'evil-ex

 ;; Rebind repeat motions
 (:after evil-snipe
   :nvm    ":"         #'evil-snipe-repeat
   :nvm    "?"         #'evil-snipe-repeat-reverse)


 ;;
 ;; Insert mode

 ;; Killing
 :gi     [S-backspace] #'delete-forward-char
 :gi     "C-d"         #'evil-delete-line
 :gi     "C-S-d"       #'evil-delete-whole-line
 :gi     "C-S-u"       #'evil-change-whole-line
 :gi     "C-S-w"       #'backward-kill-sexp

 ;; Moving faster
 :gi     "C-S-a"       #'sp-beginning-of-sexp
 :gi     "C-S-e"       #'sp-end-of-sexp
 :gi     "C-S-f"       #'sp-forward-sexp
 :gi     "C-S-b"       #'sp-backward-sexp
 :gi     "C-h"         #'sp-backward-symbol
 :gi     "C-l"         #'sp-forward-symbol
 :gi     "C-S-h"       #'backward-char
 :gi     "C-S-l"       #'forward-char
 :gi     "C-j"         #'sp-down-sexp
 :gi     "C-k"         #'sp-up-sexp
 :gi     "C-S-j"       #'sp-backward-down-sexp
 :gi     "C-S-k"       #'sp-backward-up-sexp

 ;; Basic editing
 :gi     "S-SPC"       #'tab-to-tab-stop
 ;; TODO: Tranpose last two WORDS not those around
 :gi     "C-t"         #'transpose-words
 ;; TODO: Tranpose last two SEXPS not those around
 :gi     "C-S-t"       #'transpose-sexps

 ;; company
 (:when (featurep! :completion company)
   :after company
   :i [tab] #'company-indent-or-complete-common)

 ;; yasnippet
 (:when (featurep! :feature snippets)
   ;; Bind `backtab' to `yas-expand' when snippet expansion available (it will
   ;; still call `company-yasnippet' or `helm-yas-complete' otherwise)
   :i [tab]
   (cond ((featurep! :completion company)     #'company-yasnippet)
         ((featurep! :completion helm)        #'helm-yas-complete))

   (:after yasnippet
     (:map yas-minor-mode-map
       :ig  [backtab] yas-maybe-expand
       :v   [backtab] #'yas-insert-snippet
       ;; Don't expand snippets with TAB
       :igv [tab] nil))))

(map! :prefix "g"
      :desc "Switch to last workspace" :n  [tab] #'+eduarbo/switch-to-last-workspace
      :desc "Switch to last buffer"    :n  "`"   #'evil-switch-to-windows-last-buffer
      :desc "Bookmark current buffer"  :m  "b"   #'bookmark-set
      :desc "Delete bookmark"          :m  "B"   #'bookmark-delete
      :desc "Goto char timer"          :m  "o"   #'avy-goto-char-timer
      :desc "Comment/Uncomment line"   :nv "c"   #'evil-commentary-line
      :desc "Search in project"        :nv "/"   #'+helm/project-search
      :desc "Resume last completion"   :n  "."   (cond ((featurep! :completion ivy)   #'ivy-resume)
                                                       ((featurep! :completion helm)  #'helm-resume)))

(map! :leader
      :desc "Find file in project"         ","   #'projectile-find-file
      :desc "Show top-level bindings"      "?"   #'which-key-show-top-level
      :desc "M-x"                          ";"   #'execute-extended-command
      :desc "Eval expression"              ":"   #'eval-expression
      ;; TODO: Bind to TAB
      :desc "Switch workspace"             "SPC" #'persp-switch
      :desc "Resume last search"           "`"
      (cond ((featurep! :completion ivy)         #'ivy-resume)
            ((featurep! :completion helm)        #'helm-resume))

      (:prefix ([tab] . "workspace")
        :desc "Display tab bar"            "."   #'+workspace/display
        :desc "Switch workspace"           "TAB" #'persp-switch)

      (:prefix "h"
        :desc "HEEEELP!"                   "h"   help-map)

      (:prefix "n"
        :desc "Open mode notes"            "m"   #'+eduarbo/find-notes-for-major-mode
        :desc "Open project notes"         "p"   #'+eduarbo/find-notes-for-project)

      (:prefix "p"
        :desc "Discover projects"          "d"   #'projectile-discover-projects-in-search-path)

      (:prefix "t"
        :desc "Line numbers"               "l"   #'display-line-numbers-mode
        :desc "Cycle line numbers"         "L"   #'doom/toggle-line-numbers
        :desc "Visual line mode"           "v"   #'visual-line-mode
        :desc "Subword mode"               "w"   #'subword-mode
        :desc "Frame maximized"            "M"   #'toggle-frame-maximized))
