(use-package avy
  :ensure t
  :pin melpa-stable
  :bind
  ("M-g c" . avy-goto-char)
  ("M-g w" . avy-goto-word-1)
  ("M-g l" . avy-goto-line)
  
  :config
  (setq avy-background t))

(provide 'init-avy)
