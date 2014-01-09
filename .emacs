(require 'cl) ; common lisp goodies, loop

;(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(add-to-list 'load-path "~/.emacs.d/init")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes") ; My own recipes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq el-get-user-package-directory "~/.emacs.d/el-get-init")
(setq el-get-verbose t)

; Declare packages to use
(setq el-get-packages
  '(el-get
    nyan-mode
    expand-region
    ))

(el-get 'sync el-get-packages)

(require 'init-general)
