(use-package ample-theme
  :ensure t
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample))
  :defer t)

(provide 'init-themes)
