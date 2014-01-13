(require 'ido)

(ido-mode t)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-k") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-j") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(provide 'init-ido)
