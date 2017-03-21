(require 'package)

;; Python stuff
;; https://realpython.com/blog/python/emacs-the-best-python-editor/

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    elpy
    material-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(elpy-enable)

;; 
;; --------------------------------------

;;(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally


;; This is an el-capitain related bug which displays a 'visible' bell at certain events
;; This replaces the visible bug with a message
(setq ring-bell-function (lambda () (message "*woop*")))


;; R

(add-to-list 'load-path "~/Software/ESS/lisp/")
(load "ess-site")
(setq ess-ask-for-ess-directory nil) ;; start R in current working directory


;; Julia
;; deleted for now


;; Remote Connections
;; don't make tramp prompt for cache
(require 'tramp)
(setq password-cache-expiry nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (undo-tree tabbar py-autopep8 material-theme magit julia-mode flycheck exec-path-from-shell elpy better-defaults auto-complete auctex)))
 '(python-shell-interpreter "python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; tree mode

(global-undo-tree-mode 1)
;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)
