;;; packages.el --- modeline layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eduardo Ruiz Macias <eduarbo@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst modeline-packages
  '(spaceline
    all-the-icons
    uniquify))

(defun modeline/post-init-uniquify ()
  (setq uniquify-buffer-name-style 'forward))

;; TODO add a maximized window indicator
(defun modeline/init-all-the-icons ()
  (use-package all-the-icons))

(defun modeline/post-init-spaceline ()
  (spaceline-define-segment vc-status
    "A segment to depict the current VC system with an icon"
    (cond ((string-match "Git[:-]" vc-mode) (modeline//spaceline---github-vc))
          (t (propertize (format "%s" vc-mode))))
    :when (and active vc-mode))

  (spaceline-define-segment anzu
    "Show the current match number and the total number of matches.
Requires anzu to be enabled."
    (when (and active (bound-and-true-p anzu--state))
      (concat
      (propertize (format "%s" (all-the-icons-octicon "search"))
                  'face `(:height 1 :family ,(all-the-icons-octicon-family) :inherit)
                  'display '(raise 0))
      (anzu--update-mode-line))))

  (spaceline-define-segment position-icon
    ;; (propertize (format "%s" (all-the-icons-faicon "rocket"))
    (propertize (format "%s" (all-the-icons-octicon "location"))
                'face `(:height 1 :family ,(all-the-icons-octicon-family) :inherit)
                'display '(raise 0)))

  (spaceline-define-segment mode-icon
    "A segment indicating the current buffer's mode with an icon"
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
        (propertize icon
                    'help-echo (format "Major-mode: `%s`" major-mode)
                    'display '(raise 0)
                    'face `(:height 1 :family ,(all-the-icons-icon-family-for-buffer) :inherit)))))

  (spaceline-define-segment region-info
    "Information about the size of the current selection, when applicable.
Supports both Emacs and Evil cursor conventions."
    (when (or mark-active
              (and (bound-and-true-p evil-local-mode)
                   (eq 'visual evil-state)))
      (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
             (chars (- (1+ (region-end)) (region-beginning)))
             (cols (1+ (abs (- (spaceline--column-number-at-pos (region-end))
                               (spaceline--column-number-at-pos (region-beginning))))))
             (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
             (rect (or (bound-and-true-p rectangle-mark-mode)
                       (and evil (eq 'block evil-visual-selection))))
             (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
        (concat
         (propertize (format "%s " (all-the-icons-faicon "bars"))
                     'face `(:inherit) 'display '(raise 0))
         (propertize (cond
                      (rect (format "%d×%d block" lines (if evil cols (1- cols))))
                      (multi-line (format "(%d, %d)" lines (if evil chars (1- chars))))
                      (t (format "%d chars" (if evil chars (1- chars)))))
                     'face `(:height 1 :inherit))))))

  (setq powerline-default-separator 'utf-8)
  ;; (custom-set-variables '(powerline-utf-8-separator-left #xe0c4)
  ;;                       '(powerline-utf-8-separator-right #xe0c5))

  ;; (set-face-attribute 'mode-line nil :box nil)
  (spaceline-toggle-window-number-off)

  ;; change highlight color when buffer is modified
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)

  (spaceline-install `(((mode-icon
                         buffer-id
                         remote-host)
                        :face highlight-face)

                       (persp-name
                        workspace-number
                        window-number)

                       (flycheck-error flycheck-warning flycheck-info)

                       (anzu
                        region-info
                        (position-icon point-position line-column))

                       auto-compile
                       (process :when active)
                       vc-status
                       (mu4e-alert-segment :when active)
                       (erc-track :when active)
                       (org-pomodoro :when active)
                       (org-clock :when active))

                     `(which-function
                       (python-pyvenv :fallback python-pyenv)
                       purpose
                       input-method
                       buffer-encoding-abbrev
                       buffer-size
                       (global :when active)
                       buffer-position
                       (hud :skip-alternate t)))

  (setq mode-line-format '("%e" (:eval (spaceline-ml-main)))))
