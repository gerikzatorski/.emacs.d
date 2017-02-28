;; Emacs config file

;; 1. Packages
;; 2. Misc

(add-to-list 'load-path' "~/.emacs.d/functions.el")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; ##################################################
;; 1. Packages
;; ##################################################
;;--------------------------------------------------------------------------------------------
;; Enable installation of packages from MELPA
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;--------------------------------------------------------------------------------------------
;; Adding autocomplete for Python
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Get jedi to work on macs
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;--------------------------------------------------------------------------------------------
;; Setting up the multiple-cursors package
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

;;--------------------------------------------------------------------------------------------
;; init beacon package
(beacon-mode 1)
(setq beacon-color "#666600")

;;--------------------------------------------------------------------------------------------
;; add ace jump
(add-to-list 'load-path "which-folder-ace-jump-mode-file-in/")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;--------------------------------------------------------------------------------------------
;; TODO
;; idomenu info told me to include this
(autoload 'idomenu "idomenu" nil t)
(global-set-key (kbd "M-i") 'ido-goto-symbol)

;;--------------------------------------------------------------------------------------------
;; YASnippet
(add-to-list 'load-path
	     "~/path-to-yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;--------------------------------------------------------------------------------------------
;; irony-mode for C completion
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
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; ##################################################
;; 2. Misc
;; ##################################################
;;--------------------------------------------------------------------------------------------
;; store all backup and autosave files in the tmp dir
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;--------------------------------------------------------------------------------------------
;; Stores emacs themes here
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'gruvbox t)

;;--------------------------------------------------------------------------------------------
;; Author: Patrick Gundlach 
;; nice mark - shows mark as a highlighted 'cursor' so user 'always' 
;; sees where the mark is. Especially nice for killing a region.

(defvar pg-mark-overlay nil
  "Overlay to show the position where the mark is") 
(make-variable-buffer-local 'pg-mark-overlay)

(put 'pg-mark-mark 'face 'secondary-selection)

(defvar pg-mark-old-position nil
  "The position the mark was at. To be able to compare with the
current position")

(defun pg-show-mark () 
  "Display an overlay where the mark is at. Should be hooked into 
activate-mark-hook" 
  (unless pg-mark-overlay 
    (setq pg-mark-overlay (make-overlay 0 0))
    (overlay-put pg-mark-overlay 'category 'pg-mark-mark))
  (let ((here (mark t)))
    (when here
      (move-overlay pg-mark-overlay here (1+ here)))))

(defadvice  exchange-point-and-mark (after pg-mark-exchange-point-and-mark)
  "Show visual marker"
  (pg-show-mark))

(ad-activate 'exchange-point-and-mark)
(add-hook 'activate-mark-hook 'pg-show-mark)

;;--------------------------------------------------------------------------------------------
;; change html indentation to 4
(add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)))
	  
;;--------------------------------------------------------------------------------------------
;; Custom major modes for certain files
;; .handlebars files open in html
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . html-mode))

;; open this .emacs file based on it's first line
(add-to-list 'magic-mode-alist '(";; Emacs config file" . lisp-mode) )

