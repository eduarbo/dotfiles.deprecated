;;; private/eduarbo/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+eduarbo:multi-next-line "private/eduarbo/autoload/evil" nil t)
(evil-define-motion +eduarbo:multi-next-line (count)
  "Move down 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload '+eduarbo:multi-previous-line "private/eduarbo/autoload/evil" nil t)
(evil-define-motion +eduarbo:multi-previous-line (count)
  "Move up 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload (autoload '+eduarbo:cd "private/eduarbo/autoload/evil" nil t)
(evil-define-command +eduarbo:cd ()
  "Change `default-directory' with `cd'."
  (interactive "<f>")
  (cd input))

;;;###autoload (autoload '+eduarbo:kill-all-buffers "private/eduarbo/autoload/evil" nil t)
(evil-define-command +eduarbo:kill-all-buffers (&optional bang)
  "Kill all buffers. If BANG, kill current session too."
  (interactive "<!>")
  (if bang
      (+workspace/kill-session)
    (doom/kill-all-buffers)))

;;;###autoload (autoload '+eduarbo:kill-matching-buffers "private/eduarbo/autoload/evil" nil t)
(evil-define-command +eduarbo:kill-matching-buffers (&optional bang pattern)
  "Kill all buffers matching PATTERN regexp. If BANG, only match project
buffers."
  (interactive "<a>")
  (doom/kill-matching-buffers pattern bang))
