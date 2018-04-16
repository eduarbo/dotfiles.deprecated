;;; config/eduarbo/config.el -*- lexical-binding: t; -*-

;; TODO: fix buffer switch in workspace

;; Initialize in fullscreen
(toggle-frame-fullscreen)

(let ((font "Hack"))
  (setq
   doom-font (font-spec :family font :size 14)
   doom-big-font (font-spec :family font :size 19)
   doom-unicode-font (font-spec :family font :size 15)
   doom-variable-pitch-font (font-spec :family "Fira Sans")
   ivy-posframe-font (font-spec :family font :size 16)
   ivy-height 12))

(setq user-mail-address "eduarbo@gmail.com"
      user-full-name    "Eduardo Ruiz Macias"

      ;; Set my notes directory
      +org-dir (expand-file-name "~/Google Drive/org/")

      ;; Enable accents
      ns-alternate-modifier 'none
      ;; Get some context when scrolling
      scroll-margin 10
      ;; disable line numbers
      doom-line-numbers-style nil
      ;; use gnu ls to allow dired to sort directories
      insert-directory-program "gls" dired-use-ls-dired t

      +doom-modeline-buffer-file-name-style 'relative-from-project
      show-trailing-whitespace t

      org-ellipsis " ▼ ")


;;
;; Bindings
;;

(map!
 (:after ivy
   :map ivy-minibuffer-map
   "C-f" #'+eduarbo/ivy-insert-relative-file-name
   "C-b" (+ivy-do-action! #'+eduarbo/ivy-insert-relative-file-name))

 (:leader
   (:prefix "c"
     :desc "lowerCamelCase"          :n  "c" #'+eduarbo/lower-camel-case
     :desc "UpperCamelCase"          :n  "C" #'+eduarbo/upper-camel-case
     :desc "kebab-case"              :n  "k" #'+eduarbo/dashed-words-case
     :desc "SCREAMING_SNAKE_CASE"    :n  "S" #'+eduarbo/screaming-snake-case
     :desc "snake_case"              :n  "s" #'+eduarbo/snake-case)

   (:desc "yank" :prefix "y"
     :desc "kill-ring pop"               :n "k" #'counsel-yank-pop
     :desc "buffer filename"             :n "f" #'+default/yank-buffer-filename
     :desc "buffer base name"            :n "b" #'+eduarbo/yank-buffer-base-name
     :desc "buffer name with extension"  :n "n" #'+eduarbo/yank-buffer-name
     :desc "buffer path"                 :n "p" #'+eduarbo/yank-buffer-path)))


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
  (add-hook! lisp-mode (setq-local evil-args-delimiters '(" "))))

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
