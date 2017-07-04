;;; private/eduarbo/packages.el -*- lexical-binding: t; -*-

(when (and (featurep! :feature evil)
           (featurep! :feature version-control))
  (package! evil-magit))
(when (featurep! :completion company)
  (package! company-flx))
