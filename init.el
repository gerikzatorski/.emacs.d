; -*- Mode: Emacs-Lisp -*-

;; no splash screen
(setq inhibit-startup-message t)
;; maximize screen quickly
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install use-package if necessary
;; https://github.com/jwiegley/use-package
(require 'package)
(setq package-enable-at-startup nil)

;; add repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
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
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System / Platform / OS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun osx-p ()
  "Check if a system is running OSX."
  (eq system-type 'darwin))
(defun linux-p ()
  "Check if a system is running Linux."
  (eq system-type 'gnu/linux))

;; determine operating system
;; http://ergoemacs.org/emacs/elisp_determine_OS_version.html
;; (setq windows nil mac nil linux nil)
;; (cond
;;  ((string-equal system-type "darwin") ; Mac OS X
;;   (progn
;;     (setq mac t)
;;     (message "Mac OS X")))
;;  ((string-equal system-type "gnu/linux") ; linux
;;   (progn
;;     (setq linux t)
;;     (message "Linux"))))

;; default font
(add-to-list 'default-frame-alist '(font . "Consolas" ))

;;; mac cmd key binded to meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set org variables for global todo
(setq org-agenda-files (list "~/Dropbox/org/school.org"
			     "~/Dropbox/org/home.org"))

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

(setq sentence-end-double-space nil)
(setq show-trailing-whitespace t)
(setq indicate-empty-lines t)
(setq indent-tabs-mode nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs #'y-or-n-p)

;; Make F8 be "start macro", F9 be "end macro", F10 be "execute macro"
;; (global-set-key [f8] 'start-kbd-macro)
;; (global-set-key [f9] 'end-kbd-macro)
;; (global-set-key [f10] 'call-last-kbd-macro)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :ensure    helm

  :config    (setq helm-ff-transformer-show-only-basename nil
                   helm-boring-file-regexp-list           '("\\.pyc$" "\\.git$")
                   helm-yank-symbol-first                 t
                   helm-buffers-fuzzy-matching            t
                   helm-ff-auto-update-initial-value      nil
                   helm-input-idle-delay                  0.1
                   helm-idle-delay                        0.1)

  :init      (progn
               (require 'helm-config)
               (helm-mode t)
               (helm-adaptive-mode t)

               (use-package helm-ag
                 :ensure    helm-ag
                 :bind      ("C-c a" . helm-ag))

               (use-package helm-descbinds
                 :ensure    helm-descbinds
                 :bind      ("C-h b"   . helm-descbinds))

               (use-package helm-projectile
                 :ensure    helm-projectile
		 :init      (progn
			      (require 'helm-projectile)
			      (helm-projectile-on)))
	                      ;; replaced projectile commands with helm versions

               (use-package helm-swoop
                 :ensure    helm-swoop
                 :bind      (("C-c o" . helm-swoop)
                             ("C-c M-o" . helm-multi-swoop)))

               (bind-key "C-c C-SPC" 'helm-ff-run-toggle-auto-update helm-find-files-map))

  :bind (("M-x"     . helm-M-x)
	 ("M-y"     . helm-show-kill-ring)
	 ("C-x r l" . helm-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-h i"   . helm-google-suggest)
         ("C-h a"   . helm-apropos)
         ("C-x p" .   helm-top)
         ("C-x C-b" . helm-buffers-list))

  :bind (:map helm-map
	      ("M-i" . helm-previous-line)
	      ("M-k" . helm-next-line)
	      ("M-I" . helm-previous-page)
	      ("M-K" . helm-next-page)
	      ("M-h" . helm-beginning-of-buffer)
	      ("M-H" . helm-end-of-buffer)
	      ("<tab>" . helm-execute-persistent-action)))

  ;; :diminish helm-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :bind ("C-;" . company-complete-common)
  :config
  (setq company-idle-delay nil))

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
  (add-hook 'after-init-hook 'company-statistics-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Managements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package beacon
 :ensure t
 :config
 (beacon-mode 1)
 (setq beacon-color "#666600"))

;; add themes recursively from themes directory
;; https://www.emacswiki.org/emacs/CustomThemes
(let ((basedir "~/.emacs.d/themes/"))
      (dolist (f (directory-files basedir))
        (if (and (not (or (equal f ".") (equal f "..")))
                 (file-directory-p (concat basedir f)))
            (add-to-list 'custom-theme-load-path (concat basedir f)))))

(use-package solarized-theme
  :ensure t)

(load-theme 'solarized-dark t)

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
  :mode ("\\.yasnippet" . snippet-mode))

(use-package avy
  :ensure t
  :pin melpa-stable
  :bind
  ("M-g c" . avy-goto-char)
  ("M-g w" . avy-goto-word-1)
  ("M-g l" . avy-goto-line)
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

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

;; manually installed packages
(add-to-list 'load-path "~/.emacs.d/packages/change-inner/")
(use-package change-inner
  :bind (("M-i"     . change-inner)
         ("M-o M-o" . change-outer)))

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

;; do not indent namespace text
(c-set-offset 'innamespace 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq python-shell-interpreter "/usr/local/bin/ipython3"
    python-shell-interpreter-args "--simple-prompt -i")

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
