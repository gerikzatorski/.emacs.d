(use-package ample-theme
  :ensure t
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t))
  :config (progn (enable-theme 'ample))
  :defer t)

(use-package gruvbox-theme
  :ensure t
  ;; :init (progn (load-theme 'gruvbox-dark-medium t))
  :defer t)

(provide 'init-themes)
