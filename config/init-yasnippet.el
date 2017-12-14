(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))
  :mode ("\\.yasnippet" . snippet-mode))

(provide 'init-yasnippet)
