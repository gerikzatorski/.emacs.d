(add-to-list 'load-path "~/.emacs.d/packages/change-inner/")
(use-package change-inner
  :bind (("M-i"     . change-inner)
         ("M-o M-o" . change-outer)))

(provide 'init-manual-packages)
