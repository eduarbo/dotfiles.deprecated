;; config/eduarbo/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +eduarbo/case-transform (fn beg end)
  "Apply function FN to the text between BEG and END."
  (let ((updated (funcall fn (buffer-substring beg end))))
    (goto-char beg)
    (delete-region beg end)
    (insert updated)
    (goto-char beg)))

;;;###autoload
(defun +eduarbo/case-symbol-bounds ()
  "Return a list of either the current region or current symbol bounds."
  (if (thing-at-point 'symbol)
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (list (car bounds) (cdr bounds)))
    (user-error "Point must be at a symbol")))

;;;###autoload
(defun +eduarbo/upper-camel-case (beg end)
  (interactive (+eduarbo/case-symbol-bounds))
  (+eduarbo/case-transform 's-upper-camel-case beg end))

;;;###autoload
(defun +eduarbo/lower-camel-case (beg end)
  (interactive (+eduarbo/case-symbol-bounds))
  (+eduarbo/case-transform 's-lower-camel-case beg end))

;;;###autoload
(defun +eduarbo/snake-case (beg end)
  (interactive (+eduarbo/case-symbol-bounds))
  (+eduarbo/case-transform 's-snake-case beg end))

;;;###autoload
(defun +eduarbo/screaming-snake-case (beg end)
  (interactive (+eduarbo/case-symbol-bounds))
  (+eduarbo/case-transform (lambda (s) (s-upcase (s-snake-case s))) beg end))

;;;###autoload
(defun +eduarbo/dashed-words-case (beg end)
  (interactive (+eduarbo/case-symbol-bounds))
  (+eduarbo/case-transform 's-dashed-words beg end))

;;;###autoload
(defun +eduarbo/yank-buffer-name ()
  "Copy the current buffer's name to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (file-name-nondirectory filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +eduarbo/yank-buffer-base-name ()
  "Copy the current buffer's base name to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (file-name-base filename)))
    (error "Couldn't find base filename in current buffer")))

;;;###autoload
(defun +eduarbo/yank-buffer-path ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (file-name-directory filename)))
    (error "Couldn't find base filename in current buffer")))

;; TODO: fix buffer switch in workspace
