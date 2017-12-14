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

(provide 'init-company)
