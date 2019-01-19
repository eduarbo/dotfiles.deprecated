;;; ~/.dotfiles/editor/emacs/doom/autoload/eduarbo.el -*- lexical-binding: t; -*-

(defun doom--get-modules (file)
  (unless (file-exists-p file)
    (user-error "%s does not exist" file))
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward "(doom! " nil t)
      (goto-char (match-beginning 0))
      (cdr (sexp-at-point)))))

(defun doom--put-modules (tmpfile modules)
  (with-temp-file tmpfile
    (delay-mode-hooks (emacs-lisp-mode))
    (insert (replace-regexp-in-string " " "\n" (prin1-to-string modules)))
    (indent-region (point-min) (point-max))))

;;;###autoload
(defun doom/what-has-changed ()
  "Open an ediff session to compare the module list in
~/.emacs.d/init.example.el and ~/.doom.d/init.el."
  (interactive)
  (let ((old-modules (doom--get-modules (expand-file-name "init.example.el" doom-emacs-dir)))
        (new-modules (doom--get-modules (expand-file-name "init.el" doom-private-dir)))
        (example-init-el "/tmp/doom-init.example.el")
        (private-init-el "/tmp/doom-private-init.el"))
    (doom--put-modules example-init-el old-modules)
    (doom--put-modules private-init-el new-modules)
    (ediff private-init-el example-init-el)))

;;;###autoload
(defun +eduarbo/find-notes-for-major-mode (&optional arg)
  "TODO"
  (interactive "P")
  (let ((default-directory (expand-file-name "code/" org-directory)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat (string-remove-suffix "-mode" (symbol-name major-mode)) ".org"))))))

;;;###autoload
(defun +eduarbo/find-notes-for-project (&optional arg)
  "TODO"
  (interactive "P")
  (let ((project-root (doom-project-name))
        (default-directory (expand-file-name "projects/" org-directory)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat project-root ".org"))))))

;;;###autoload
(defun +eduarbo/switch-to-last-workspace ()
  "Switch to previously selected workspace, if it exists."
  (interactive)
  (unless (eq 'non-existent
              (gethash +workspace--last
                       *persp-hash* 'non-existent))
    (+workspace/switch-to +workspace--last)))

;;;###autoload
(defun +eduarbo/omni-helm-mini ()
  (interactive)
  (let ((helm-mini-default-sources
         '(helm-source-buffers-list
           helm-source-recentf
           helm-source-bookmarks
           helm-source-buffer-not-found)))
    (helm-mini)))
