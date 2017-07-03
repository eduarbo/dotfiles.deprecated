;;; private/eduarbo/autoload/eduarbo.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +eduarbo/install-snippets ()
  "Install my snippets from https://github.com/hlissner/emacs-snippets into
private/eduarbo/snippets."
  (interactive)
  (doom-fetch :github "hlissner/emacs-snippets"
              (expand-file-name "snippets" (doom-module-path :private 'eduarbo))))

;;;###autoload
(defun +eduarbo/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +eduarbo/region-or-nil ()
  "Return region whenever available."
  (when (use-region-p)
    (deactivate-mark)
    (buffer-substring-no-properties (mark) (point))))

;;;###autoload
(defun +eduarbo/swiper-region ()
  "Swiper region whenever available."
  (interactive)
  (swiper (+eduarbo/region-or-nil)))

;;;###autoload
(defun +eduarbo/counsel-git-grep-region ()
  "Git grep region whenever available."
  (interactive)
  (counsel-git-grep nil (+eduarbo/region-or-nil)))

;;;###autoload
(defun +eduarbo/rename-this-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (and (configuration-layer/package-usedp 'projectile)
                          (projectile-project-p))
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))


(defmacro +eduarbo-def-finder! (name dir)
  "Define a pair of find-file and browse functions."
  `(progn
     (defun ,(intern (format "+eduarbo/find-in-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'projectile-find-file))))
     (defun ,(intern (format "+eduarbo/browse-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'find-file))))))

;;;###autoload (autoload '+eduarbo/find-in-templates "private/eduarbo/autoload/eduarbo" nil t)
;;;###autoload (autoload '+eduarbo/browse-templates "private/eduarbo/autoload/eduarbo" nil t)
(+eduarbo-def-finder! templates +file-templates-dir)

;;;###autoload (autoload '+eduarbo/find-in-snippets "private/eduarbo/autoload/eduarbo" nil t)
;;;###autoload (autoload '+eduarbo/browse-snippets "private/eduarbo/autoload/eduarbo" nil t)
(+eduarbo-def-finder! snippets +eduarbo-snippets-dir)

;;;###autoload (autoload '+eduarbo/find-in-dotfiles "private/eduarbo/autoload/eduarbo" nil t)
;;;###autoload (autoload '+eduarbo/browse-dotfiles "private/eduarbo/autoload/eduarbo" nil t)
(+eduarbo-def-finder! dotfiles (expand-file-name ".dotfiles" "~"))

;;;###autoload (autoload '+eduarbo/find-in-emacsd "private/eduarbo/autoload/eduarbo" nil t)
;;;###autoload (autoload '+eduarbo/browse-emacsd "private/eduarbo/autoload/eduarbo" nil t)
(+eduarbo-def-finder! emacsd doom-emacs-dir)

;;;###autoload (autoload '+eduarbo/find-in-notes "private/eduarbo/autoload/eduarbo" nil t)
;;;###autoload (autoload '+eduarbo/browse-notes "private/eduarbo/autoload/eduarbo" nil t)
(+eduarbo-def-finder! notes +org-dir)
