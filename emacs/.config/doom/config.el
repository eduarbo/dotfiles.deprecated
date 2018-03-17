;;; config/eduarbo/config.el -*- lexical-binding: t; -*-

;; TODO: fix buffer switch in workspace

;;
;; Bindings
;;

(map!
 :nv "go"  #'evil-avy-goto-char-timer
 :nv "Q"   #'fill-paragraph

 (:leader
   (:prefix "c"
     :desc "lowerCamelCase"          :n  "c" #'+eduarbo/lower-camel-case
     :desc "UpperCamelCase"          :n  "C" #'+eduarbo/upper-camel-case
     :desc "kebab-case"              :n  "k" #'+eduarbo/dashed-words-case
     :desc "SCREAMING_SNAKE_CASE"    :n  "S" #'+eduarbo/screaming-snake-case
     :desc "snake_case"              :n  "s" #'+eduarbo/snake-case)

   (:desc "yank" :prefix "k"
     :desc "kill-ring pop"               :n "k" #'counsel-yank-pop
     :desc "buffer filename"             :n "f" #'+default/yank-buffer-filepath
     :desc "buffer base name"            :n "b" #'+eduarbo/yank-buffer-base-name
     :desc "buffer name with extension"  :n "n" #'+eduarbo/yank-buffer-name
     :desc "buffer path"                 :n "p" #'+eduarbo/yank-buffer-path)))


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

(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))

;; feature/evil
(after! evil-mc
  ;; Make evil-mc resume its cursors when I switch to insert mode
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

;; FIXME: not sure why the hooks are not setting the buffer var
(after! evil-args
  ;; Set space as a delimiter arguments for lisp-family languages
  (add-hook! 'lisp-mode-hook (setq-local evil-args-delimiters '(" ")))
  (add-hook! 'emacs-lisp-mode-hook (setq-local evil-args-delimiters '(" "))))

;; lang/org
(after! org-bullets
  ;; The standard unicode characters are usually misaligned depending on the
  ;; font. This bugs me. Personally, markdown #-marks for headlines are more
  ;; elegant, so we use those.
  (setq org-bullets-bullet-list '("#")))

;; TODO: before uncomment this, figure out why pos-tip is not showing up in
;; first place

;; (after! flycheck-pos-tip
;;   ;; make sure flycheck errors take precedence over eldoc. Eldoc delay is 0.5
;;   (setq flycheck-display-errors-delay 0.6)
;;   ;; Disable broken tooltip in macOS
;;   (flycheck-pos-tip-mode -1))

;; source my snippets
(after! yasnippet
  (push (expand-file-name "snippets/" +private-config-path) yas-snippet-dirs))
