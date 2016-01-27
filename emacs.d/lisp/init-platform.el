;;; init-platform --- Platform-specific settings
;;; Commentary:

;;; Code:
(require 'init-fonts)

;; This must run after window setup or it seems to have no effect.
(add-hook 'window-setup-hook
          (lambda ()
            (when (memq window-system '(mac ns))
              ;; Nextstep interface (used on GNUstep and Mac OS X)
              (add-to-list 'default-frame-alist '(font . "Hack"))
              (set-face-attribute 'default nil :font "Hack")
              (sanityinc/set-frame-font-size 14)
              (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen))

            (when (memq window-system '(x))
              ;; If using X
              (add-to-list 'default-frame-alist '(font . "Hack"))
              (set-face-attribute 'default nil :font "Hack")
              (sanityinc/set-frame-font-size 16))

            (when (fboundp 'powerline-reset)
              (powerline-reset))))

;; Display emoji on Macs where the font is already there.
(when (memq window-system '(mac))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(provide 'init-platform)
;;; init-platform.el ends here
