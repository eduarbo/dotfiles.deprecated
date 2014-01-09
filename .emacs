(require 'cl) ; common lisp goodies, loop
(require 'init-benchmarking) ;; Measure startup time

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-user-package-directory "~/.emacs.d/packages")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/recipes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq el-get-verbose t)

(setq el-get-sources
      '((:name expand-region
       :before (global-set-key (kbd "C-@") 'er/expand-region))))

(setq el-get-packages
      '(el-get
	nyan-mode
	expand-region))

(setq my-packages
      '(nyan-mode))

(setq dim-packages
      (append
       el-get-packages
       my-packages
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync dim-packages)

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA"      . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(load-theme 'odersky t)

; Nyan mode
(require 'nyan-mode)
(setq-default nyan-wavy-trail t)
(nyan-mode)
(nyan-start-animation)



