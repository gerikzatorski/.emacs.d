; -*- Mode: Emacs-Lisp -*-

;; First things first
(setq inhibit-startup-message t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Gotta do this early
(package-initialize)
;; marmalade signature issue
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; Simple settings
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq visible-bell 1)
(delete-selection-mode 1)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs #'y-or-n-p)

;; Global Key Bindings
(global-set-key [f2] (lambda nil (interactive) (revert-buffer nil t t) (message (concat "Reverted buffer " (buffer-name)))))
(global-set-key [f5] 'compile)
(global-set-key [f8] 'neotree-toggle)

;; Font Settings
(set-face-attribute 'default nil :height 150)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install use-package if necessary
(require 'package)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; ensure use-package installs packages
(setq use-package-always-ensure t)

(use-package ansi-color
  ;; :after (ansi-term compile)
  :config
  (defun my/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package neotree
  :config
  (setq-default neo-smart-open t)
  (setq-default neo-dont-be-alone t))

(use-package ag
  :config
  (add-to-list 'ag-arguments "--word-regexp")
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package company
  :diminish ""
  :config
  (global-company-mode t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

(use-package company-irony
  :after company
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-jedi
  :after company
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-to-list 'company-backends 'company-jedi))

(use-package flycheck
  :init
  (global-flycheck-mode t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (x-focus-frame nil)
  (exec-path-from-shell-initialize))

(use-package magit
  :bind
  ("C-c m s" . magit-status)
  ("C-c m u" . magit-clone-url))

(use-package ivy
  :diminish 'ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  :bind
  ("C-c C-r" . ivy-resume))

(use-package swiper
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("M-i" . counsel-imenu)
  ("M-y" . counsel-yank-pop))

(use-package counsel-etags
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

(use-package avy
  :bind
  ("C-;" . avy-goto-char)
  ("C-'" . avy-goto-word-or-subword-1)
  :config
  (setq avy-background t))

(use-package ace-window
  :bind* ("M-o" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package projectile
  :pin melpa-stable
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package multiple-cursors
  :bind
  ("C-c m c" . mc/edit-lines)
  ("C-c m n" . mc/insert-numbers)
  ("C-c m l" . mc/insert-letters)
  ("C-c m a" . mc/mark-all-like-this))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

;; Themes

(use-package naquadah-theme
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-eighties))
  ;; :defer t)

(use-package ample-theme
  :defer t)

(use-package apropospriate-theme
  :defer t)

;; Modes

(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\)$" . yaml-mode))

(use-package nxml-mode
  :mode ("\\.\\(xml\\|launch\\)$" . nxml-mode))

(use-package cmake-mode
  :defer t
  :mode "CMakeLists.txt")

(use-package markdown-mode
  :defer t
  :mode ("\\.\\(markdown\\|mdown\\|md\\)$" . markdown-mode))

(use-package dockerfile-mode
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package groovy-mode
  :defer t
  :config
  ;; Indent groovy code four spaces instead of two
  (defun my-groovy-mode-hook ()
    (setq c-basic-offset 4))
  (add-hook 'groovy-mode-hook #'my-groovy-mode-hook)
  :mode ("Jenkinsfile\\'" . groovy-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load all files in defuns-dir
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
