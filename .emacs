(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;tramp shit
;; I really don't know which one of these incantantions makes
;; the magic happen (loading R remotely via ESS and tramp), but here they are
(require 'tramp)
(require 'tramp-sh)

(tramp-set-completion-function "ssh"
   '((tramp-parse-sconfig "/etc/ssh_config")
     (tramp-parse-sconfig "~/.ssh/config")))

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(add-to-list 'tramp-remote-process-environment
             (format "DISPLAY=%s" (getenv "DISPLAY")))

(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

(require 'ess-site)
(load "ess-site.el")

(load "auctex.el" nil t t)
(setq TeX-PDF-mode t)
(show-paren-mode t)
(setq ess-ask-for-ess-directory nil)
(setq inferior-ess-own-frame t)

;; This is the magic line to encourage new frames
(setq split-window-preferred-function nil)


;; Show filename in title bar
;;(setq frame-title-format "%b - Emacs")

;; Rembember password when logging in from tramp
(setq password-cache-expiry nil)

(require 'ido)
(ido-mode t)

(require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode t)


;;(let ((default-directory "~/.emacs.d/elpa/"))
;;    (normal-top-level-add-to-load-path '("."))
;;(normal-top-level-add-subdirs-to-load-path))

;;(require 'python-mode)
;;(setq py-shell-switch-buffers-on-execute-p t)
;;(setq py-switch-buffers-on-execute-p t)
; don't split windows
;;(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
;;(setq py-smart-indentation t)

;;(eval-after-load "python"
;;  '(progn
;;     (define-key python-mode-map (kbd "C-c C-r") 'python-shell-send-region)))

;;(require 'tabbar nil t)
;;(tabbar-mode 1)
;;(global-set-key [M-left] 'tabbar-backward-tab)
;;(global-set-key [M-right] 'tabbar-forward-tab)
(put 'downcase-region 'disabled nil)
