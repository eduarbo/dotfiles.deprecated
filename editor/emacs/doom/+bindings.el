;;; ~/.dotfiles/editor/emacs/doom/+bindings.el -*- lexical-binding: t; -*-

(map!
 :nv     "SPC"         #'+evil/fold-toggle
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

 ;; Easier window navigation
 :en "C-h"             #'evil-window-left
 :en "C-j"             #'evil-window-down
 :en "C-k"             #'evil-window-up
 :en "C-l"             #'evil-window-right

 ;; Switch leader, access to command mode and repeat motions
 (:after evil-snipe
   :map evil-snipe-parent-transient-map
   ";"                 nil
   ","                 nil)
 :nvm    ":"           #'evil-snipe-repeat
 :nvm    "?"           #'evil-snipe-repeat-reverse
 :nvm    ";"           #'evil-ex

 (:when (featurep! :completion company)
   ;; FIXME: Autoload company-indent-or-complete-common
   ;; Figure out a way to override the def-package! macro to autoload the
   ;; command with the :commands keyword and be able to remap it
   ;; :i [tab] #'company-indent-or-complete-common)
   :i [tab] #'company-complete-common)

 (:when (featurep! :feature snippets)
   (:after yasnippet
     (:map yas-minor-mode-map
       :ig [tab] nil
       :v  [tab] nil
       :ig [backtab] yas-maybe-expand
       :v  [backtab] #'yas-insert-snippet)))

 (:prefix "g"
   :desc "Switch to last workspace" :n  [tab] #'+workspace:switch-previous
   :desc "Evil commentary line"     :n  "C"   #'evil-commentary-line
   :desc "Switch to last buffer"    :n  "l"   #'evil-switch-to-windows-last-buffer
   :desc "Goto char timer"          :m  "o"   #'avy-goto-char-timer
   :desc "Describe at point"        :nv "h"   #'helpful-at-point
   :desc "Search in project"        :nv "/"   #'+helm/project-search)

 (:leader
   :desc "M-x"                      :nv ";"   #'execute-extended-command
   :desc "Eval expression"          :nv ":"   #'eval-expression
   :desc "Toggle last popup"        "\""      #'+popup/toggle

   (:prefix "h"
     :desc "HEEEELP!"               :n  "h"   help-map)

   (:prefix "n"
     :desc "Open mode notes"       "m" #'+eduarbo/find-notes-for-major-mode
     :desc "Open project notes"    "p" #'+eduarbo/find-notes-for-project)

   (:prefix "t"
     :desc "Line numbers"           :n  "l"   #'display-line-numbers-mode
     :desc "Cycle line numbers"     :n  "L"   #'doom/toggle-line-numbers
     :desc "Visual line mode"       :n  "v"   #'visual-line-mode
     :desc "Frame maximized"        :n  "M"   #'toggle-frame-maximized)))

