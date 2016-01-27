;;; Flycheck mode:
(add-hook 'flycheck-mode-hook
          (lambda ()
            (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
            (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; Python mode:
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;;; Magit mode (which does not open in evil-mode):
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

;;; Git Commit Mode (a Magit minor mode):
(add-hook 'git-commit-mode-hook 'evil-insert-state)

;;; Emmet mode:
(add-hook 'emmet-mode-hook
          (lambda ()
            (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
            (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point)))

;;; Web mode:
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-style-padding 2)
            (yas-minor-mode t)
            (emmet-mode)
            (flycheck-add-mode 'html-tidy 'web-mode)
            (flycheck-mode)))

(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "css")
                 (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;;; Emacs Lisp mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;(yas-minor-mode-on)
            (turn-on-eldoc-mode)
            (highlight-symbol-mode)))

;;; SH mode:
(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 2)
                          (setq sh-indentation 2)))

;;; Let me move the selection like a normal human in the Grizzl results buffer.
(add-hook 'grizzl-mode-hook (lambda ()
                              (define-key *grizzl-keymap* (kbd "C-j") 'grizzl-set-selection-1)
                              (define-key *grizzl-keymap* (kbd "C-k") 'grizzl-set-selection+1)))

;;; Javascript mode:
(add-hook 'javascript-mode-hook (lambda ()
                                  (set-fill-column 120)
                                  (turn-on-auto-fill)
                                  (setq js-indent-level 2)))

;;; Markdown mode:
(add-hook 'markdown-mode-hook (lambda ()
                                (turn-on-auto-fill)
                                (flyspell-mode)))

;;; HTML mode:
(add-hook 'html-mode-hook (lambda ()
                            (setq sgml-basic-offset 2)))


(add-hook 'before-save-hook
          '(lambda ()
             (untabify (point-min) (point-max))
             (delete-trailing-whitespace)))


(provide 'init-hooks)
