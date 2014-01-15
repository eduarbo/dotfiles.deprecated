(load-theme 'molokai t)

(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq visible-bell t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      scroll-preserve-screen-position t
      inhibit-startup-message t
      inhibit-splash-screen t
      color-theme-is-global t
      sentence-end-double-space nil
      mouse-yank-at-point t
      ring-bell-function 'ignore
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      diff-switches "-u")


;; Highlight current line
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline nil)

;; org-mode colors
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
        ))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Unclutter the modeline
(require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "tagedit" '(diminish 'tagedit-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "skewer-mode" '(diminish 'skewer-mode))
(eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
(eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "drag-stuff" '(diminish 'drag-stuff-mode))
(eval-after-load "volatile-highlights" '(diminish 'volatile-highlights-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")

(provide 'appearance)
