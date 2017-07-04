;;; private/eduarbo/autoload/workspaces.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +eduarbo/set-workspace--last (name frame)
  "Save last visited workspace."
  (let ((old-name (+workspace-current-name)))
    (when (and (framep frame)
               (not (string= old-name persp-nil-name))
               (not (string= old-name +workspace--last)))
      (setq +workspace--last old-name))))
