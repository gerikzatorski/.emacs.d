;; Emacs config file

;; User Info
(setq user-full-name "Gerik Zatorski")

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

;; handle .el files
(add-to-list 'load-path' "~/.emacs.d/functions.el")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Set the path variable
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))


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



;; Miscellaneous Stuff

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
