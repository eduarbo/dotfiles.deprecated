(setq user-full-name "Eduardo Ruiz"
      user-mail-address "eduarbo@gmail.com")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
"Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
(dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
"Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
(let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
        (push (match-string group str) result)
        (setq pos (match-end group)))
    result))

(defun sanityinc/string-rtrim (str)
"Remove trailing whitespace from `STR'."
(replace-regexp-in-string "[ \t\n]+$" "" str))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun sanityinc/directory-of-library (library-name)
"Return the directory in which the `LIBRARY-NAME' load file is found."
(file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
"Delete the current file, and kill the buffer."
(interactive)
(or (buffer-file-name) (error "No file is currently being edited"))
(when (yes-or-no-p (format "Really delete '%s'?"
                            (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
"Renames both current buffer and file it's visiting to NEW-NAME."
(interactive "sNew name: ")
(let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
    (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
    (message "A buffer named '%s' already exists!" new-name)
    (progn
        (when (file-exists-p filename)
        (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))


;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
"Open the current file as a URL using `browse-url'."
(interactive)
(let ((file-name (buffer-file-name)))
    (if (tramp-tramp-file-p file-name)
    (error "Cannot open tramp file")
    (browse-url (concat "file://" file-name)))))


(defun smart-open-line ()
"Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
(interactive)
(move-end-of-line nil)
(newline-and-indent))


(defun smart-open-line-above ()
"Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
(interactive)
(move-beginning-of-line nil)
(newline-and-indent)
(forward-line -1)
(indent-according-to-mode))


(defun smarter-move-beginning-of-line (arg)
"Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
(interactive "^p")
(setq arg (or arg 1))

;; Move lines first
(when (/= arg 1)
    (let ((line-move-visual nil))
    (forward-line (1- arg))))

(let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
    (move-beginning-of-line 1))))

(defun find-user-init-file ()
"Edit the `user-init-file', in another window."
(interactive)
(find-file-other-window user-init-file))

(defun smart-kill-whole-line (&optional arg)
"A simple wrapper around `kill-whole-line' that respects indentation."
(interactive "P")
(kill-whole-line arg)
(back-to-indentation))

(defun indent-buffer ()
"Indent the currently visited buffer."
(interactive)
(indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
"Indent a region if selected, otherwise the whole buffer."
(interactive)
(save-excursion
    (if (region-active-p)
        (progn
        (indent-region (region-beginning) (region-end))
        (message "Indented selected region."))
    (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(defun comment-eclipse ()
    (interactive)
    (let ((start (line-beginning-position))
            (end (line-end-position)))
        (when (region-active-p)
        (setq start (save-excursion
                        (goto-char (region-beginning))
                        (beginning-of-line)
                        (point))
                end (save-excursion
                    (goto-char (region-end))
                    (end-of-line)
                    (point))))
        (comment-or-uncomment-region start end)))

(defun kill-region-or-backward-word ()
(interactive)
(if (region-active-p)
    (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun copy-to-end-of-line ()
(interactive)
(kill-ring-save (point)
                (line-end-position))
(message "Copied to end of line"))

(defun copy-line (arg)
"Copy to end of line, or as many lines as prefix argument"
(interactive "P")
(if (null arg)
    (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun save-region-or-current-line (arg)
(interactive "P")
(if (region-active-p)
    (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(require 'htmlfontify)
(defun fontify-and-browse ()
"Fontify the current buffer into HTML, write it to a temp file, and open it in a browser."
(interactive)
(let* ((fontified-buffer (hfy-fontify-buffer))
        (temp-file-name (make-temp-file "ff" nil ".html")))
    (with-current-buffer fontified-buffer
                        (write-region (point-min) (point-max) temp-file-name))
    (browse-url (concat "file://" temp-file-name))))

(defadvice
load-theme (after restore-line-numbering)
"Re-set linum-format after loading themes, which frequently overwrite it."
(defvar linum-format 'my-linum-relative-line-numbers))
(ad-activate 'load-theme)

(defun chrome-reload (&optional focus)
"Use osascript to tell Google Chrome to reload.
If optional argument FOCUS is non-nil, give Chrome the focus as well."
(interactive "P")
(let ((cmd (concat "osascript -e 'tell application \"Google Chrome\" "
                    "to (reload (active tab of (window 1)))"
                    (if focus " & activate" "")
                    "'")))
    (shell-command cmd "*Reload Chrome")))

(defun load-only-theme (theme)
"Disable all themes and then load THEME interactively."
(interactive
    (list
    (completing-read "Load custom theme: "
                    (mapcar 'symbol-name
                            (custom-available-themes)))))
(mapcar #'disable-theme custom-enabled-themes)
(load-theme (intern theme) nil nil)
(when (fboundp 'powerline-reset)
    (powerline-reset)))

(defun func-region (func start end)
"Run FUNC over the region between START and END in current buffer."
(save-excursion
    (let ((text (delete-and-extract-region start end)))
    (insert (funcall func text)))))

(defun hex-region (start end)
"Hexify (URL encod) the region between START and END in current buffer."
(interactive "r")
(func-region #'url-hexify-string start end))

(defun unhex-region (start end)
"Unhex (URL decode) the region between START and END in current buffer."
(interactive "r")
(func-region #'url-unhex-string start end))

(defun cycle-powerline-separators (&optional reverse)
"Set Powerline separators in turn.  If REVERSE is not nil, go backwards."
(interactive)
(let* ((fn (if reverse 'reverse 'identity))
        (separators (funcall fn '("arrow" "arrow-fade" "slant"
                                "chamfer" "wave" "brace" "roundstub" "zigzag"
                                "butt" "rounded" "contour" "curve")))
        (found nil))
    (while (not found)
        (progn (setq separators (append (cdr separators) (list (car separators))))
                (when (string= (car separators) powerline-default-separator)
                    (progn (setq powerline-default-separator (cadr separators))
                        (setq found t)
                        (redraw-display)))))))

(defun occur-last-search ()
"Run `occur` with the last evil search term."
(interactive)
;; Use the appropriate search term based on regexp setting.
(let ((term (if evil-regexp-search
                (car-safe regexp-search-ring)
                (car-safe search-ring))))
    ;; If a search term exists, execute `occur` on it.
    (if (> (length term) 0)
    (occur term)
    (message "No term to search for."))))

(defun show-first-occurrence ()
"Display the location of the word at point's first occurrence in the buffer."
(interactive)
(save-excursion
    (let ((search-word (thing-at-point 'symbol t)))
    (goto-char 1)
    (re-search-forward search-word)
    (message (concat
                "L" (number-to-string (line-number-at-pos)) ": "
                (replace-regexp-in-string
                "[ \t\n]*\\'"
                ""
                (thing-at-point 'line t)
                ))))))

(defun switch-to-previous-buffer ()
"Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
(interactive)
(switch-to-buffer (other-buffer (current-buffer) 1)))

(defun selective-display-increase ()
(interactive)
(set-selective-display
    (if selective-display (+ selective-display 1) 1)))

(defun selective-display-decrease ()
(interactive)
(when selective-display
    (set-selective-display
    (if (< (- selective-display 1) 1)
        nil
        (- selective-display 1)))))

(defun my-align-single-equals ()
"Align on the first single equal sign."
(interactive)
(align-regexp
    (region-beginning) (region-end)
    "\\(\\s-*\\)=" 1 1 nil))

;;; Helpers for narrowing.
(defun narrow-and-set-normal ()
"Narrow to the region and, if in a visual mode, set normal mode."
(interactive)
(narrow-to-region (region-beginning) (region-end))
(if (string= evil-state "visual")
    (progn (evil-normal-state nil)
        (evil-goto-first-line))))

;;; From http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
(defun wikipedia-search (search-term)
"Search for SEARCH-TERM on wikipedia"
(interactive
    (let ((term (if mark-active
                (buffer-substring (region-beginning) (region-end))
                (word-at-point))))
    (list (read-string (format "Wikipedia (%s): " term) nil nil term))))
(w3m-browse-url (concat
                    "http://en.m.wikipedia.org/w/index.php?search="
                    search-term)))

(defun air--pop-to-file (file &optional split)
"Visit a FILE, either in the current window or a SPLIT."
(if split
    (find-file-other-window file)
    (find-file file)))

(defun air-pop-to-org-todo (split)
"Visit my main TODO list, in the current window or a SPLIT."
(interactive "P")
(air--pop-to-file "~/Dropbox/org/todo.org" split))

(defun air-pop-to-org-notes (split)
"Visit my main notes file, in the current window or a SPLIT."
(interactive "P")
(air--pop-to-file "~/Dropbox/org/notes.org" split))

(defun air-pop-to-org-vault (split)
"Visit my encrypted vault file, in the current window or a SPLIT."
(interactive "P")
(air--pop-to-file "~/Dropbox/org/vault.gpg" split))

(defun air-pop-to-org-agenda (split)
"Visit the org agenda, in the current window or a SPLIT."
(interactive "P")
(org-agenda-list)
(when (not split)
    (delete-other-windows)))

(use-package gruvbox-theme
  :ensure t
  :init
    (load-theme 'gruvbox t))

(defun switch-theme (theme)
  "Disables any currently active themes and loads THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "s-<f12>" 'switch-theme)
(bind-key "s-<f11>" 'disable-active-themes)

(add-to-list 'default-frame-alist
             '(font . "Hack-12"))

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Acents
(require 'iso-transl)

;; Increase number of undo
(setq undo-limit 100000)

;; Allow all disabled commands
(setq disabled-command-hook nil)

;; Seed the random-number generator
(random t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep all backup and auto-save files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(setq-default indent-tabs-mode nil)

;; Turn off the blinking cursor
(blink-cursor-mode -1)

;; Never insert tabs
(setq-default indent-tabs-mode nil)
(set-default 'tab-width 2)

(setq-default indicate-empty-lines t)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode t)
(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Transparently open compressed files
(auto-compression-mode t)

;; UTF-8 please
(set-language-environment    'utf-8)

;; Auto refresh buffers
(global-auto-revert-mode 1)

(column-number-mode t)

;; Wrap line
(global-visual-line-mode)
(diminish 'visual-line-mode)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; -i gets alias definitions from .bash_profile
(setq shell-command-switch "-ic")

;; Don't beep at me
(setq visible-bell t)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 10000)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Highlight current line
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline nil)

;; Rename modelines
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
                    '(defadvice ,mode (after rename-modeline activate)
                                (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")

;; Misc
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      scroll-preserve-screen-position t
      inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      color-theme-is-global t
      mouse-yank-at-point t
      ring-bell-function 'ignore
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      diff-switches "-u")

(defun vsplit-other-window ()
  "Splits the window vertically and switches to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))

(defun hsplit-other-window ()
  "Splits the window horizontally and switches to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode t)

(global-set-key (kbd "<F1>") 'help-map)
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)
(global-set-key [(shift return)] 'smart-open-line)
(global-set-key [(control shift return)] 'smart-open-line-above)

(defun air--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-key
    ","  'helm-projectile
    "<"  'helm-projectile-switch-project
    "n"  'helm-find-files
    "b"  'helm-mini             ;; Switch to another buffer
    "m"  'avy-goto-char-2
    "."  'switch-to-previous-buffer
    ":"  'eval-expression
    "c"  'evilnc-comment-or-uncomment-lines
    "d"  (lambda () (interactive) (evil-ex-call-command nil "bdelete" nil))
    "D"  'open-current-line-in-codebase-search
    "l"  'whitespace-mode       ;; Show invisible characters
    "o"  'delete-other-windows  ;; C-w o
    "A"  'ag-project            ;; Ag search from project's root
    "f"  'swiper-helm
    "r"  'chrome-reload
    "R"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display))
    "w"  'delete-trailing-whitespace
    "t"  'gtags-reindex
    "T"  'gtags-find-tag
    "x"  'helm-M-x
    "v"  'hsplit-other-window
    "s"  'vsplit-other-window)

  ;; Magit mappings
  (evil-leader/set-key
    "gs"  'magit-status
    "gb"  'magit-blame-toggle)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame))))

(defun air--config-evil ()
  "Configure evil mode."

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
                  flycheck-error-list-mode
                  git-rebase-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Use insert state in these additional modes.
  (dolist (mode '(magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings ag-mode-map 'normal
    "n"   'evil-search-next
    "N"   'evil-search-previous
    "RET" 'compile-goto-error)

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-w C-w") 'other-window)

  (defun next-conflict-marker ()
    (interactive)
    (evil-next-visual-line)
    (if (not (search-forward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-max) t))
        (evil-previous-visual-line))
    (move-beginning-of-line nil))

  (defun previous-conflict-marker ()
    (interactive)
    (search-backward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-min) t)
    (move-beginning-of-line nil))

  ;; Global bindings.

  ;; Window motions
  (define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)

  (define-key evil-normal-state-map (kbd ";")     'evil-ex)
  (define-key evil-normal-state-map (kbd "C-]")   'gtags-find-tag-from-here)
  (define-key evil-normal-state-map (kbd "g/")    'occur-last-search)
  (define-key evil-normal-state-map (kbd "[i")    'show-first-occurrence)

  (define-key evil-insert-state-map (kbd "C-a")   'smarter-move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e")   'end-of-line) ;; I know...
  (define-key evil-insert-state-map (kbd "S-h")   'smarter-move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "S-l")   'end-of-line)

  ;; Treat wrapped line scrolling as single lines
  (define-key evil-normal-state-map (kbd "j")     'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k")     'evil-previous-visual-line)

  (define-key evil-normal-state-map (kbd "C-u") (lambda ()
                                                  (interactive)
                                                  (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-d") (lambda ()
                                                  (interactive)
                                                  (evil-scroll-down nil)))

  (evil-define-key 'normal org-mode-map (kbd "]n") 'org-forward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "[n") 'org-backward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "C-S-l") 'org-shiftright)
  (evil-define-key 'normal org-mode-map (kbd "C-S-h") 'org-shiftleft)
  (evil-define-key'insert org-mode-map (kbd "C-S-l") 'org-shiftright)
  (evil-define-key 'insert org-mode-map (kbd "C-S-h") 'org-shiftleft)

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)

  ;; Don't move back the cursor one position when exiting insert mode
  (setq evil-move-cursor-back nil))

(use-package evil
  :ensure t
  :config
  (add-hook 'evil-mode-hook 'air--config-evil)
  (evil-mode 1)

  (use-package evil-escape
    :diminish evil-escape-mode
    :ensure t
    :config
    (setq-default evil-escape-key-sequence "jk")
    :init
    (evil-escape-mode))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (air--config-evil-leader))

  (use-package evil-jumper
    :ensure t
    :config
    (global-evil-jumper-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

  (use-package evil-visualstar
    :ensure t
    :config
    (global-evil-visualstar-mode))

  (use-package evil-indent-textobject
    :ensure t))

(let ((is-mac (string-equal system-type "darwin")))
  (when is-mac
    ;; delete files by moving them to the trash
    (setq delete-by-moving-to-trash t)
    (setq trash-directory "~/.Trash")

    ;; Don't make new frames when opening a new file with Emacs
    (setq ns-pop-up-frames nil)

    ;; set the Fn key as the hyper key
    (setq ns-function-modifier 'hyper)

    ;; Use Command-` to switch between Emacs windows (not frames)
    (bind-key "s-`" 'other-window)

    ;; Use Command-Shift-` to switch Emacs frames in reverse
    (bind-key "s-~" (lambda() () (interactive) (other-window -1)))

    ;; Because of the keybindings above, set one for `other-frame'
    (bind-key "s-1" 'other-frame)

    ;; Fullscreen!
    (setq ns-use-native-fullscreen nil) ; Not Lion style
    (bind-key "<s-return>" 'toggle-frame-fullscreen)
    ;; Start maximized
    (custom-set-variables
      '(initial-frame-alist (quote ((fullscreen . maximized)))))

    ;; buffer switching
    (bind-key "s-{" 'previous-buffer)
    (bind-key "s-}" 'next-buffer)

    ;; Compiling
    (bind-key "H-c" 'compile)
    (bind-key "H-r" 'recompile)
    (bind-key "H-s" (defun save-and-recompile () (interactive) (save-buffer) (recompile)))

    ;; disable the key that minimizes emacs to the dock because I don't
    ;; minimize my windows
    ;; (global-unset-key (kbd "C-z"))

    (defun open-dir-in-finder ()
      "Open a new Finder window to the path of the current buffer"
      (interactive)
      (shell-command "open ."))
    (bind-key "s-/" 'open-dir-in-finder)

    (defun open-dir-in-iterm ()
      "Open the current directory of the buffer in iTerm."
      (interactive)
      (let* ((iterm-app-path "/Applications/iTerm.app")
             (iterm-brew-path "/opt/homebrew-cask/Caskroom/iterm2/1.0.0/iTerm.app")
             (iterm-path (if (file-directory-p iterm-app-path)
                             iterm-app-path
                           iterm-brew-path)))
        (shell-command (concat "open -a " iterm-path " ."))))
    (bind-key "s-=" 'open-dir-in-iterm)

    ;; Not going to use these commands
    (put 'ns-print-buffer 'disabled t)
    (put 'suspend-frame 'disabled t)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; make ibuffer the default buffer lister.
(defalias 'list-buffers 'ibuffer)

(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(use-package recentf
  :init
  (progn
    (recentf-mode t)
    (setq recentf-max-saved-items 200)))

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/Dropbox/Agenda"))))

(bind-key "C-c c" 'org-capture)
(setq org-default-notes-file "~/Dropbox/Notes/notes.org")

(setq org-use-speed-commands t)

(setq org-image-actual-width 550)

(setq org-tags-column 45)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (calc . t)
   (latex . t)
   (java . t)
   (ruby . t)
   (scheme . t)
   (sh . t)
   (sqlite . t)
   (js . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  "Do not confirm evaluation for these languages."
  (not (or (string= lang "C")
           (string= lang "java")
           (string= lang "python")
           (string= lang "emacs-lisp")
           (string= lang "sqlite"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(use-package ox-pandoc
  :no-require t
  :ensure t)

;; mdfind is the command line interface to Spotlight
(setq locate-command "mdfind")

(bind-key "C-x m" 'shell)
(bind-key "C-x M" 'ansi-term)

(use-package avy
  :ensure t
  :commands avy-goto-char-2)

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?;))
  (ace-window-display-mode)
  :bind ("s-o" . ace-window))

(use-package c-eldoc
  :commands c-turn-on-eldoc-mode
  :ensure t
  :init (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode))

(use-package clojure-mode
  :ensure t)

(use-package company
  :diminish company-mode
  :init
  (global-company-mode)
  :config
  ;(setq company-tooltip-common-selection ((t (:inherit company-tooltip-selection :background "yellow2" :foreground "#c82829"))))
  ;(setq company-tooltip-selection ((t (:background "yellow2"))))
  (setq company-idle-delay 0.5)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package dictionary
    :ensure t)

(use-package ein
  :ensure t)

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :config
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package evil-nerd-commenter
  :ensure t
  :commands evilnc-comment-or-uncomment-lines)

(use-package helm
  :ensure t
  :diminish helm-mode
  :init (progn
          (require 'helm-config)

          (use-package helm-projectile
            :ensure t
            :commands helm-projectile helm-projectile-switch-project
            :bind ("C-c p h" . helm-projectile))

          (use-package helm-ag :ensure t)

          (use-package swiper-helm :ensure t)

          (use-package helm-themes :ensure t)

          (setq helm-locate-command "mdfind -interpret -name %s %s"
                helm-buffers-fuzzy-matching t
                helm-ff-newfile-prompt-p nil
                helm-M-x-fuzzy-match t)
          (helm-mode)))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(use-package expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

(use-package flycheck
  :ensure t
  :config
    (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
    (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)
    (setq flycheck-html-tidy-executable "tidy5")
    ;; Override default flycheck triggers
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
          flycheck-idle-change-delay 0.8)

    (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package gist
  :ensure t
  :commands gist-list)

(use-package macrostep
  :ensure t
  :bind ("H-`" . macrostep-expand))

(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)))

(use-package mmm-mode
  :defer t
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-classes
   '((markdown-cl
      :submode emacs-lisp-mode
      :face mmm-declaration-submode-face
      :front "^```cl[\n\r]+"
      :back "^```$")))
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-cl))

(use-package multiple-cursors
  :ensure t
  :init (require 'multiple-cursors)
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-!"         . mc/mark-next-symbol-like-this)
         ("s-d"         . mc/mark-all-dwim)))

;; Preset width nlinum
(use-package nlinum
  :ensure t
  :config
    (global-nlinum-mode t)
    (setq nlinum-format "%d "))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands projectile-mode
  :config
  (progn
    (projectile-global-mode t)
    (setq projectile-enable-caching t)))

(use-package rainbow-mode
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.restclient\\'" . restclient-mode))

(use-package scratch
  :ensure t)

(use-package skewer-mode
  :commands skewer-mode
  :ensure t
  :config (skewer-setup))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config (progn (require 'smartparens-config)
                 (smartparens-global-mode t)))

(sp-local-pair 'org-mode "~" "~" :actions '(wrap))
(sp-local-pair 'org-mode "/" "/" :actions '(wrap))
(sp-local-pair 'org-mode "*" "*" :actions '(wrap))

(use-package smartscan
  :ensure t
  :config (global-smartscan-mode 1)
  :bind (("s-n" . smartscan-symbol-go-forward)
         ("s-p" . smartscan-symbol-go-backward)))

(use-package smooth-scrolling
  :ensure t)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

(use-package swiper
  :ensure t
  :commands swiper)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-mode-lighter ""
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" user-emacs-directory))))
  (global-undo-tree-mode))

(use-package which-key
  :diminish ""
  :config
  (which-key-mode t))

(use-package wgrep
  :ensure t
  :init
  (use-package ag
    :ensure t
    :config
    (add-hook 'ag-mode-hook
                (lambda ()
                (wgrep-ag-setup)
                (define-key ag-mode-map (kbd "n") 'evil-search-next)
                (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
    (setq ag-executable "/usr/local/bin/ag")
    (setq ag-highlight-search t)
    (setq ag-reuse-buffers t)
    (setq ag-reuse-window t))
  (use-package wgrep-ag
    :ensure t
    :commands (wgrep-ag-setup)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (yas-global-mode))

(use-package zoom-frm
  :ensure t
  :bind (("C-=" . zoom-in/out)
         ("C-z" . toggle-zoom-frame))
  :config
  (setq frame-zoom-font-difference 10))

(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)   ; Curly braces alignment
  (c-set-offset 'case-label 4))         ; Switch case statements alignment

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'java-mode-hook 'my-c-mode-hook)

(setq display-time-default-load-average nil)

(setq battery-mode-line-format "[%b%p%% %t]")

(use-package doc-view
  :config
  (define-key doc-view-mode-map (kbd "<right>") 'doc-view-next-page)
  (define-key doc-view-mode-map (kbd "<left>") 'doc-view-previous-page))

(setq mouse-wheel-scroll-amount (quote (0.01)))

(use-package visible-mode
  :bind ("H-v" . visible-mode))
