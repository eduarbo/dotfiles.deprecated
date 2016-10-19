;;; packages.el --- Crypt Layer packages File for Spacemacs.
;;
;; Copyright (C) 2016  Eduardo Ruiz
;;
;; Author: Eduardo Ruiz <eduarbo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst crypt-packages
  '((org :location built-in)
    (org-crypt :location built-in
               :toggle (configuration-layer/package-usedp 'org))))

(defun crypt/init-org-crypt ()
  (use-package org-crypt
    :after org
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "U" 'org-decrypt-entry  ; Mnemonic of Unravel
      "E" 'org-encrypt-entries)
    :config
    ;; Encrypt all entries before saving
    (org-crypt-use-before-save-magic)
    (setq org-crypt-tag-matcher "crypt"
          org-tags-exclude-from-inheritance '("crypt")
          org-crypt-key crypt-gpg-key
          org-crypt-disable-auto-save 'encrypt
          epa-file-encrypt-to org-crypt-key
          ;; use gpg2 to cache the passphrase with gpg-agent, otherwise it won't work
          epg-gpg-program "gpg2")))
