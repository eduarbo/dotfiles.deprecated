;;; config/eduarbo/packages.el -*- lexical-binding: t; -*-

(package! evil-escape :ignore t)
(package! emacs-snippets :ignore t)

(when (and (featurep! :feature evil)
           (featurep! :feature version-control))
  (package! evil-magit))
