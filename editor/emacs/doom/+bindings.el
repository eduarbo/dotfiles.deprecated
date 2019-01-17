;;; ~/.dotfiles/editor/emacs/doom/+bindings.el -*- lexical-binding: t; -*-

(map!
 :gnvime "M-;"         #'execute-extended-command

 ;; navigate between buffers & workspaces
 :gnvime "C-,"         #'previous-buffer
 :gnvime "C-."         #'next-buffer
 :gnvime "C-<"         #'+workspace/switch-left
 :gnvime "C->"         #'+workspace/switch-right

 ;; go to next/prev shorcuts like in other apps
 :gnvime "M-["         #'previous-buffer
 :gnvime "M-]"         #'next-buffer
 :gnvime "M-{"         #'+workspace/switch-left
 :gnvime "M-}"         #'+workspace/switch-right

 (:when (featurep! :ui popup)
   :gnvime "C-`"       #'+popup/toggle)

 :gnvime "C-~"
 (cond ((featurep! :completion ivy)   #'ivy-resume)
       ((featurep! :completion helm)  #'helm-resume))

 ;; Easier window navigation
 :en     "C-h"         #'evil-window-left
 :en     "C-j"         #'evil-window-down
 :en     "C-k"         #'evil-window-up
 :en     "C-l"         #'evil-window-right

 :nv     "SPC"         #'+evil/fold-toggle

 ;; Swap leader, access to command mode and repeat motions
 :nvm    ":"           #'evil-snipe-repeat
 :nvm    "?"           #'evil-snipe-repeat-reverse
 :nvm    ";"           #'evil-ex
 (:after evil-snipe
   :map evil-snipe-parent-transient-map
   ";"                 nil
   ","                 nil)


 ;;
 ;; Insert mode

 :gi     "C-'"         #'helpful-key

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
   ;; FIXME: Autoload company-indent-or-complete-common
   ;; Figure out a way to override the def-package! macro to autoload the
   ;; command with the :commands keyword and be able to remap it
   ;; :i [tab] #'company-indent-or-complete-common)
   :i [tab] #'company-complete-common)

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
       :igv [tab] nil)))

 (:prefix "g"
   :desc "Switch workspace"         :n  [tab] #'persp-switch
   :desc "Switch to last buffer"    :n  "l"   #'evil-switch-to-windows-last-buffer
   :desc "Goto char timer"          :m  "o"   #'avy-goto-char-timer
   :desc "Evil commentary line"     :n  "C"   #'evil-commentary-line
   :desc "Search in project"        :nv "/"   #'+helm/project-search)

 (:leader
   :desc "Show top-level bindings"      "?"   #'which-key-show-top-level
   :desc "M-x"                          ";"   #'execute-extended-command
   :desc "Eval expression"              ":"   #'eval-expression
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

   (:prefix "t"
     :desc "Line numbers"               "l"   #'display-line-numbers-mode
     :desc "Cycle line numbers"         "L"   #'doom/toggle-line-numbers
     :desc "Visual line mode"           "v"   #'visual-line-mode
     :desc "Subword mode"               "w"   #'subword-mode
     :desc "Frame maximized"            "M"   #'toggle-frame-maximized)))
