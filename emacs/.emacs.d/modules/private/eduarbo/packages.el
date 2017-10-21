;;; private/eduarbo/packages.el -*- lexical-binding: t; -*-

(when (and (featurep! :feature evil)
           (featurep! :feature version-control))
  (package! evil-magit))

;; prevent package from being installed
(package! evil-escape :ignore t)
