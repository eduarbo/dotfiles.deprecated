;;; ~/.dotfiles/editor/emacs/doom/+bindings.el -*- lexical-binding: t; -*-

(map!
 :nv     "SPC"         #'+helm/workspace-mini
 :gnvime "M-;"         #'execute-extended-command
 :gnvime [C-return]    #'+popup/toggle

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

 ;; Free the leader!
 :nvm    ":"           #'evil-snipe-repeat-reverse
 (:after evil-snipe
   :map evil-snipe-parent-transient-map
   ","                 nil)

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
   :desc "Evil Ex"                  :nv ","   #'evil-ex

   (:prefix "h"
     :desc "HEEEELP!"               :n  "h"   help-map)

   (:prefix "t"
     :desc "Line numbers"           :n  "l"   #'display-line-numbers-mode
     :desc "Cycle line numbers"     :n  "L"   #'doom/toggle-line-numbers
     :desc "Visual line mode"       :n  "v"   #'visual-line-mode
     :desc "Frame maximized"        :n  "M"   #'toggle-frame-maximized)))

