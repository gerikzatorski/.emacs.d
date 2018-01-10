;; currently giving me hassle on multiple machines
;; (use-package ample-theme
;;   :ensure t
;;   :init (progn (load-theme 'ample t t)
;;                (load-theme 'ample-flat t t)
;;                (load-theme 'ample-light t t))
;;   :defer t)

(use-package gruvbox-theme
  :ensure t
  :init (progn (load-theme 'gruvbox-dark-medium t))
  :defer t)

(provide 'init-themes)
