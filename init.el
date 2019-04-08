; -*- Mode: Emacs-Lisp -*-

;; First things first
(setq inhibit-startup-message t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Gotta do this early
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
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

;; Install use-package if necessary
(require 'package)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ansi-color
  :ensure t
  ;; :after (ansi-term compile)
  :config
  (defun my/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker)
  :diminish
  :init
  (use-package docker-image     :commands docker-images)
  (use-package docker-container :commands docker-containers)
  (use-package docker-volume    :commands docker-volumes)
  (use-package docker-network   :commands docker-containers)
  (use-package docker-machine   :commands docker-machines))

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose.*\.yml\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas/global-mode)
         ("C-c y m" . yas/minor-mode)
         ("C-c y r" . yas-reload-all)
         ("C-c y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-load-directory "~/.emacs.d/snippets")
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package neotree
  :ensure t
  :config
  (setq-default neo-smart-open t)
  (setq-default neo-dont-be-alone t))

(use-package ag
  :ensure t
  :config
  (add-to-list 'ag-arguments "--word-regexp")
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package company
  :ensure t
  :diminish ""
  :config
  (global-company-mode)
  :custom
  (company-idle-delay 1.0)
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-show-numbers t))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (x-focus-frame nil)
  (exec-path-from-shell-initialize))

;; Use rtags for navigation
(use-package rtags
  :ensure t
  :hook
  (c-mode-common-hook . rtags-start-process-unless-running)
  (c++-mode-common-hook . rtags-start-process-unless-running))

(use-package magit
  :ensure t
  :bind
  ("C-c m s" . magit-status)
  ("C-c m u" . magit-clone-url))

(use-package ivy
  :ensure t
  :diminish 'ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  :bind
  ("C-c C-r" . ivy-resume))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("M-i" . counsel-imenu)
  ("M-y" . counsel-yank-pop))

(use-package avy
  :ensure t
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-word-or-subword-1)
  :config
  (setq avy-background t))

(use-package ace-window
  :ensure t
  :bind* ("M-o" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-mode))

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

;; Themes

(use-package naquadah-theme
  ;; :init (load-theme 'naquadah t)
  :defer t
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-eighties)
  ;; :defer t
  :ensure t)

(use-package ample-theme
  :defer t
  :ensure t)

(use-package apropospriate-theme
  :defer t
  :ensure t)

;; Modes

(use-package yaml-mode
  :ensure t
  :mode ("\\.\\(yml\\|yaml\\)$" . yaml-mode))

(use-package nxml-mode
  :mode ("\\.\\(xml\\|launch\\)$" . nxml-mode))

(use-package cmake-mode
  :ensure t
  :defer t
  :mode "CMakeLists.txt")

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.\\(markdown\\|mdown\\|md\\)$" . markdown-mode))

(use-package groovy-mode
  :ensure t
  :defer t
  :config
  ;; Indent groovy code four spaces instead of two
  (defun my-groovy-mode-hook ()
    (setq c-basic-offset 4))
  (add-hook 'groovy-mode-hook #'my-groovy-mode-hook)
  :mode ("Jenkinsfile\\'" . groovy-mode))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
