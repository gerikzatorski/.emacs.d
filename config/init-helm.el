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

(provide 'init-helm)
