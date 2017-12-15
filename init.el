; -*- Mode: Emacs-Lisp -*-

;; Filename: init.el
;; Description: Gerik Zatorski's emacs configuration
;; Author: Gerik Zatorski

;; TABLE OF CONTENTS
;; Basic Settings
;; Key Bindings
;; Packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maximize screen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; no splash screen 
(setq inhibit-startup-message t)

;; set org variables for global todo
(setq org-agenda-files (list "~/Dropbox/org/school.org"
			     "~/Dropbox/org/home.org"))

;; store all backup and autosave files in the system's temp dir
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; sentences end with a single space, not two
(setq sentence-end-double-space nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs #'y-or-n-p)

;; Custom configuration set by Emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bind common commands to function keys
(global-set-key [f1] 'help-command)
;; (global-set-key [f2] 'undo)
;; (global-set-key [f7] 'save-buffer)

;; Make F8 be "start macro", F9 be "end macro", F10 be "execute macro"
(global-set-key [f8] 'start-kbd-macro)
(global-set-key [f9] 'end-kbd-macro)
(global-set-key [f10] 'call-last-kbd-macro)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install use-package if necessary
;; https://github.com/jwiegley/use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
	    (exec-path-from-shell-initialize)))

;; config collections
(load "~/.emacs.d/config/init-company.el")
(load "~/.emacs.d/config/init-themes.el")
(load "~/.emacs.d/config/init-helm.el")

;; config singles
(load "~/.emacs.d/config/init-yasnippet.el")
(load "~/.emacs.d/config/init-avy.el")
(load "~/.emacs.d/config/init-beacon.el")
(load "~/.emacs.d/config/init-mc.el")
(load "~/.emacs.d/config/init-ace-window.el")
(load "~/.emacs.d/config/init-expand-region.el")
(load "~/.emacs.d/config/init-projectile.el")
(load "~/.emacs.d/config/init-magit.el")

;; config modes
(load "~/.emacs.d/config/init-irony.el")
(load "~/.emacs.d/config/init-markdown.el")

;; config misc
(load "~/.emacs.d/config/init-functions.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode based on extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .ino files open in C
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;; .launch files open in xml
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my-c-indent-setup ()
;;   (c-set-offset 'innamespace 0)
;; (add-hook 'c-mode-hook 'my-c-indent-setup)

;; (defconst my-cc-style
;;   '("cc-mode"
;;     (c-offsets-alist . ((innamespace . [0])))))
;; (c-add-style "my-cc-mode" my-cc-style)
;; (add-hook 'c++-mode-hook (lambda () (c-set-style "my-cc-style") ) )

(c-set-offset 'innamespace 0)
