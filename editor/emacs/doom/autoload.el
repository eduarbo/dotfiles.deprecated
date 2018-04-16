;; config/eduarbo/autoload.el -*- lexical-binding: t; -*-

(require 's)
(require 'thingatpt)

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

;;;###autoload
(defun +eduarbo/file-name-make-relative (filename reference)
  (interactive)
  (let ((reduced-path-reference)
        (common-pos 0)
        (depth 0)
        (pos 0)
        (retval ""))
    (while (eq (aref filename common-pos) (aref reference common-pos))
      (setq common-pos (+ common-pos 1)))
    (setq reduced-path-reference (substring reference (+ common-pos 1)))
    (while (< pos (length (substring reference (+ common-pos 1))))
      (if (eq (aref reduced-path-reference pos) (aref "/" 0))
          (setq depth (+ depth 1)))
      (setq pos (+ pos 1)))
    (dotimes (i depth)
      (setq retval (concat retval "../")))
    (setq retval (concat retval (substring filename common-pos)))
    retval))

;;;###autoload
(defun +eduarbo/ivy-insert-relative-file-name ($string &optional $from $to)
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'symbol)))
       (list nil (car bds) (cdr bds)))))

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if $string t nil))
    (setq inputStr (if workOnStringP $string (buffer-substring-no-properties $from $to)))
    (setq outputStr
          (file-name-sans-extension (+eduarbo/file-name-make-relative (concat (doom-project-root) x) (buffer-file-name))))

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region $from $to)
        (goto-char $from)
        (insert outputStr)))))

