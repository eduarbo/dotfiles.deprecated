(require 'windmove)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c |") 'toggle-window-split)

(global-set-key (kbd "S-C-h") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-l") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-j") 'shrink-window)
(global-set-key (kbd "S-C-k") 'enlarge-window)

(global-set-key (kbd "C-c d") 'duplicate-line)

(global-set-key (kbd "M-s") 'save-buffer)

(global-set-key (kbd "M-o") 'smart-open-line)
(global-set-key (kbd "M-O") 'smart-open-line-above)

(global-set-key (kbd "C-^") 'top-join-line)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; Shortcut to instant access to .emacs init file
(global-set-key (kbd "C-c E") 'find-user-init-file)

;; Remmaping Kill Whole Line
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)

;; Go Back to Previous Window
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; (global-set-key (kbd "C-S-h") 'backward-char)
;; (global-set-key (kbd "C-S-j") 'next-line)
;; (global-set-key (kbd "C-S-k") 'previous-line)
;; (global-set-key (kbd "C-S-l") 'forward-char)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "s-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") (lambda () (save-region-or-current-line 1)))

(provide 'key-bindings)
