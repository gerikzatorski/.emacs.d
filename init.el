; -*- Mode: Emacs-Lisp -*-

;; no splash screen
(setq inhibit-startup-message t)
;; maximize screen quickly
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; hide toolbar
(tool-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install use-package if necessary
;; https://github.com/jwiegley/use-package
(require 'package)
(setq package-enable-at-startup nil)

;; add repositories
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; setup load-paths and autoloads for installed packages
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; needed for use-package key bindings
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System/Platform/OS Specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun osx-p ()
  "Check if a system is running OSX."
  (eq system-type 'darwin))
(defun linux-p ()
  "Check if a system is running Linux."
  (eq system-type 'gnu/linux))
(defun win-p ()
  "Check if a system is running Windows."
  (eq system-type 'gnu/windows-nt))

;;; mac cmd key binded to meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; store all backup and autosave files in the system's temp dir
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Custom configuration set by Emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; highlights paired parens
(show-paren-mode 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs #'y-or-n-p)

;; other
(setq sentence-end-double-space nil)
(setq show-trailing-whitespace t)
(setq indicate-empty-lines t)
(setq indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode))
  ;; :config
  ;; ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; ;; irony-mode's buffers by irony-mode's function
  ;; (defun my-irony-mode-hook ()
  ;;   (define-key irony-mode-map [remap completion-at-point]
  ;;     'irony-completion-at-point-async)
  ;;   (define-key irony-mode-map [remap complete-symbol]
  ;;     'irony-completion-at-point-async))
  ;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  ;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package markdown-mode
  :ensure    t
  :defer     t
  :mode      ("\\.\\(markdown\\|mdown\\|md\\)$" . markdown-mode))

(use-package groovy-mode
  :ensure t
  :config
  (defun my-groovy-mode-hook ()
    ;; Indent groovy code four spaces instead of two
    (setq c-basic-offset 4))
  (add-hook 'groovy-mode-hook #'my-groovy-mode-hook)
  :mode
  (("Jenkinsfile\\'" . groovy-mode)))

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

(use-package csharp-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set a default font
;; (when (member "Consolas" (font-family-list))
;;   (set-face-attribute 'default nil :font "Consolas"))

;; (set-frame-font "Consolas" nil t)

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

;; visualize color codes
(use-package rainbow-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
	    (exec-path-from-shell-initialize)))

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

(use-package avy
  :ensure t
  :pin melpa-stable
  :bind
  ("C-;" . avy-goto-char-2)
  :config
  (setq avy-background t))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c m c" . mc/edit-lines)
  ("C-c m n" . mc/insert-numbers)
  ("C-c m l" . mc/insert-letters)
  ("C-c m a" . mc/mark-all-like-this))

(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; (use-package expand-region
;;   :ensure t
;;   :bind
;;   ("C-=" . er/expand-region)
;;   ("C--" . er/contract-region))

;; (use-package google-this
;;   :ensure t
;;   :diminish
;;   :config
;;   (google-this-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;            Org Mode Stuff           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq org-startup-folded nil)

(use-package org-bullets
    :ensure t
    :config
    (setq org-bullets-bullet-list '("∙"))
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;             Test Packges            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; manually installed packages
;; (add-to-list 'load-path "~/.emacs.d/packages/change-inner/")
;; (use-package change-inner
;;   :bind (("M-i"     . change-inner)
;;          ("M-o M-o" . change-outer)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;    Major mode based on extension    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .ino files open in C
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;; .launch files open in xml
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; do not indent namespace text
(c-set-offset 'innamespace 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Comment Line
;; http://stackoverflow.com/a/9697222/3105650
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region-or-line)

;; Rename Buffer (deletes file with old name)
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Duplicate Current Line
;; https://stackoverflow.com/a/998472/3105650
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))
(global-set-key (kbd "C-c d") 'duplicate-line)

;; Delete File and Buffer
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))
(global-set-key (kbd "C-x <deletechar>")  'delete-file-and-buffer)

;; Find User Init File
;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)
