; -*- Mode: Emacs-Lisp -*-

;; Turn off mouse interface early in startup to avoid momentary display
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(package-initialize)

;; no splash screen
(setq inhibit-startup-message t)

;;; mac cmd key binded to meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; highlights paired parens
(show-paren-mode 1)

;; one space after periods
(setq sentence-end-double-space nil)

;; use spaces instead of tabs  when indenting
(setq-default indent-tabs-mode nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs #'y-or-n-p)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SOURCES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOOTSTRAP USE-PACKAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install use-package if necessary
;; https://github.com/jwiegley/use-package
(require 'package)
(setq package-enable-at-startup nil)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; needed for use-package key bindings
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))
  :mode ("\\.yasnippet" . snippet-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t))

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t  ; add recent files and bookmarks to the ivy-switch-buffer
        ivy-count-format "%d/%d ") ; ivy prompt formatting
  :bind
  ("C-c C-r" . ivy-resume))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper)  ; remapping isearch-forward
  ("C-r" . swiper)) ; remapping isearch-backward

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)) ; remapping M-x

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1))

(use-package company
  :ensure t
  :diminish
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay t)

  (use-package company-irony-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony-c-headers))

  (use-package company-irony
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony))

  (use-package company-jedi
    :ensure t
    :init
    (setq company-jedi-python-bin "python2")
    :config
    (add-to-list 'company-backends 'company-jedi))

  (use-package company-web
    :ensure t
    :bind ("C-c w" . company-web-html)
    :config
    (add-to-list 'company-backends 'company-web-html))

  (use-package company-statistics
    :ensure t
    :config
    (add-hook 'after-init-hook 'company-statistics-mode)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy))

(use-package magit
  :ensure t
  :bind     (("C-x gs" . magit-status)
             ("C-x gc" . magit-clone-url)))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c m c" . mc/edit-lines)
  ("C-c m n" . mc/insert-numbers)
  ("C-c m l" . mc/insert-letters)
  ("C-c m a" . mc/mark-all-like-this))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

;; NAVIGATION

(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy
  :ensure t
  :pin melpa-stable
  :bind
  ("C-;" . avy-goto-char-2)
  :config
  (setq avy-background t))

;; Modes

(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode))

(use-package markdown-mode
  :ensure    t
  :defer     t
  :mode      ("\\.\\(markdown\\|mdown\\|md\\)$" . markdown-mode))

(use-package groovy-mode
  :ensure t
  :config
  ;; Indent groovy code four spaces instead of two
  (defun my-groovy-mode-hook ()
    (setq c-basic-offset 4))
  (add-hook 'groovy-mode-hook #'my-groovy-mode-hook)
  :mode
  (("Jenkinsfile\\'" . groovy-mode)))

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

;; GUI Stuff

(use-package ample-theme
  :ensure t
  :init (load-theme 'ample t))

(use-package apropospriate-theme
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-color "#666600"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUTURE PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package smex
;;   :ensure t)

;; TODO
;; (use-package ivy-hydra
;;   :ensure t)

;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode t))

;; (use-package neotree
;;   :ensure t
;;   :bind (("<f2>" . neotree-toggle))
;;   :init
;;   (progn
;;     ;; Every time when the neotree window is opened, it will try to find current
;;     ;; file and jump to node.
;;     (setq-default neo-smart-open t)
;;     ;; Do not allow neotree to be the only open window
;;     (setq-default neo-dont-be-alone t))
;;   :config
;;   (setq neo-theme 'nerd)) ; 'classic, 'nerd, 'ascii, 'arrow

;; (use-package org-bullets
;;     :ensure t
;;     :config
;;     (setq org-bullets-bullet-list '("∙"))
;;     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .ino files open in C
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;; .launch files open in xml
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.test\\'" . xml-mode))

(org-babel-do-load-languages
      'org-babel-load-languages
      '((C . t)))

;; resize frame font
(set-face-attribute 'default (selected-frame) :height 160)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
