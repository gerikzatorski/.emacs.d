(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'init-ace-window)

;; unbind M-p in markdown mode?
;; (add-hook 'org-mode-hook
;;       (lambda ()
;;         (local-unset-key (kbd "C-c C-b"))))
