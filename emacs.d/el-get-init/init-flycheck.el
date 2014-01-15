(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Elisp stuff
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
