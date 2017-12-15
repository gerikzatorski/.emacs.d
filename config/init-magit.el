(use-package magit

  :ensure t

  :bind     (("C-x gs" . magit-status)
             ("C-x gc" . magit-clone-url)))

(provide 'init-magit)
