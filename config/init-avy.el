(use-package avy
  :ensure t
  :pin melpa-stable
  :bind
  ("M-'" . avy-goto-word-or-subword-1)
  :config
  (setq avy-background t))

(provide 'init-avy)
