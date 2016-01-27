;;; init-maps.el -- Provide global key maps

;;; Commentary:
;;; Provide global maps that aren't specific to any mode or package.

;;; Code:

(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)
(global-set-key [(shift return)] 'smart-open-line)
(global-set-key [(control shift return)] 'smart-open-line-above)

(define-key global-map (kbd "C-c c") (lambda () (interactive) (org-capture nil "a")))
(define-key global-map (kbd "C-x C-q") 'kill-emacs)
(define-key global-map (kbd "C-c C-u") 'insert-char) ;; "u" for Unicode, get it?
(define-key global-map (kbd "C-c l")   'dictionary-lookup-definition)
(define-key global-map (kbd "C-c d f") 'find-name-dired)

;; C-v is "visual block" in normal mode, but use it for "paste" in insert mode.
(when (equal system-type 'darwin)
  (evil-define-key 'insert global-map (kbd "C-v") 'yank))

(provide 'init-maps)
;;; init-maps.el ends here
