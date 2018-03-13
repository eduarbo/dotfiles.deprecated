;;; config/eduarbo/config.el -*- lexical-binding: t; -*-

(setq +doom-modeline-buffer-file-name-style 'relative-from-project
      show-trailing-whitespace t)

(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))


;;
;; Bindings
;;

(map!
 :nv "go"  #'evil-avy-goto-char-timer
 :nv "Q"   #'fill-paragraph

 (:leader
   (:desc "yank" :prefix "k"
     :desc "kill-ring pop"               :n "k" #'counsel-yank-pop
     :desc "buffer filename"             :n "f" #'+default/yank-buffer-filepath
     :desc "buffer base name"            :n "b" #'+eduarbo/yank-buffer-base-name
     :desc "buffer name with extension"  :n "n" #'+eduarbo/yank-buffer-name
     :desc "buffer path"                 :n "p" #'+eduarbo/yank-buffer-path)))


;;
;; Packages
;;

(def-package! evil-magit
  :when (and (featurep! :feature evil)
             (featurep! :feature version-control))
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t
        magit-diff-paint-whitespace t))


;;
;; Config
;;

;; lazy-load `evil-easymotion'
(map! :m "gs" #'+default/easymotion)
(defun +default/easymotion ()
  (interactive)
  (let ((prefix (this-command-keys)))
    (evilem-default-keybindings prefix)
    (map! :map evilem-map
          "n" (evilem-create #'evil-ex-search-next)
          "N" (evilem-create #'evil-ex-search-previous)
          "s" (evilem-create #'evil-snipe-repeat
                             :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                             :bind ((evil-snipe-scope 'buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight)))
          "S" (evilem-create #'evil-snipe-repeat-reverse
                             :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                             :bind ((evil-snipe-scope 'buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight))))
    (set-transient-map evilem-map)
    (which-key-reload-key-sequence prefix)))


;;
;; Modules
;;

;; feature/evil
(after! evil-mc
  ;; Make evil-mc resume its cursors when I switch to insert mode
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

(after! evil-args
  ;; Set space as a delimiter arguments for lisp-family languages
  (add-hook! 'lisp-mode-hook (setq-local evil-args-delimiters '(" ")))
  (add-hook! 'emacs-lisp-mode-hook (setq-local evil-args-delimiters '(" "))))

(after! magit
  (setq ;magit-commit-show-diff nil
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(after! company
  (setq completion-ignore-case t
        company-abort-manual-when-too-short t
        company-dabbrev-ignore-case t))

;; lang/org
(after! org-bullets
  ;; The standard unicode characters are usually misaligned depending on the
  ;; font. This bugs me. Personally, markdown #-marks for headlines are more
  ;; elegant, so we use those.
  (setq org-bullets-bullet-list '("#")))

(after! yasnippet
  (push (expand-file-name "snippets/") yas-snippet-dirs))
