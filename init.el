;; Emacs config file

;; User Info
(setq user-full-name "Gerik Zatorski")


;; Set path to dependencies
(setq settings-dir (expand-file-name "settings" user-emacs-directory))

;; Setup load path
(add-to-list 'load-path settings-dir)

;; handle other .el files
(add-to-list 'load-path' "~/.emacs.d/functions.el")

;; keep emacs Custom-settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; ------------------------------------------------------------
;; Basic Settings
;; ------------------------------------------------------------

;; replaces region when typing
(pending-delete-mode t)

;; no splash screen 
(setq inhibit-startup-message t)

;; ------------------------------------------------------------
;; Packages
;; ------------------------------------------------------------

;; Install use-package if necessary
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Set the path variable
(use-package exec-path-from-shell
  :ensure t
  :config
  ;; setup environment variables from the user's shell
  (exec-path-from-shell-initialize))

;;; Packages
(use-package jedi
  :ensure t
  :config
  (autoload 'jedi:setup "jedi" nil t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c m c") 'mc/edit-lines))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-color "#666600"))

(use-package ace-jump-mode
  :ensure t
  :config
  (add-to-list 'load-path "which-folder-ace-jump-mode-file-in/")
  (require 'ace-jump-mode)
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; TODO
;; idomenu info told me to include this
;;(autoload 'idomenu "idomenu" nil t)
;;(global-set-key (kbd "M-i") 'ido-goto-symbol)

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'load-path "~/path-to-yasnippet")
  (require 'yasnippet)
  (yas-global-mode 1))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package gruvbox-theme
  :ensure t
  :init
  (load-theme 'gruvbox t))

(use-package go-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; ------------------------------------------------------------
;; Miscellaneous Stuff
;; ------------------------------------------------------------

;; store all backup and autosave files in the tmp dir
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; change html indentation to 4
(add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)))
	  
;; Custom major modes for certain files
;; .handlebars files open in html
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . html-mode))

;; open this .emacs file based on it's first line
(add-to-list 'magic-mode-alist '(";; Emacs config file" . lisp-mode) )
