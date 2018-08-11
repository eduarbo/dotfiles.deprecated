;;; ~/.dotfiles/editor/emacs/doom/+bindings.el -*- lexical-binding: t; -*-

(map!
 :gnvime [C-escape]    #'execute-extended-command
 :gnvime [C-return]    #'+popup/toggle
 :gnvime "C-,"         #'previous-buffer
 :gnvime "C-."         #'next-buffer
 :gnvime "C-<"         #'+workspace/switch-left
 :gnvime "C->"         #'+workspace/switch-right
 :gnvime "M-;"         #'execute-extended-command

 ;; :desc "Ex command"                :nv ";"  #'evil-ex
 ;; :desc "Ex command"                :nv ":"  #'evil-ex-search-next

 (:prefix "g"
   :desc "Switch to last workspace" :n  [tab] #'+workspace:switch-previous
   :desc "Evil commentary line"     :n  "C"   #'evil-commentary-line
   :desc "Switch to last buffer"    :n  "l"   #'evil-switch-to-windows-last-buffer
   :desc "Goto char timer"          :m  "o"   #'avy-goto-char-timer
   :desc "Describe at point"        :nv "h"   #'helpful-at-point
   :desc "Search in project"        :nv "/"   #'+helm/project-search)

 (:leader
   (:prefix "h"
     :desc "HEEEELP!"              :n "h"  help-map)
   (:prefix "t"
     :desc "Frame maximized"       :n "M" #'toggle-frame-maximized)))
