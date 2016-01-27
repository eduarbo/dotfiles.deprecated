;;; init-utils --- Handy functions
;;; Commentary:

;;; Code:

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

(provide 'init-utils)
;;; init-utils.el ends here
