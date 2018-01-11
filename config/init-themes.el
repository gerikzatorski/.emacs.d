;; add themes recursively from themes directory
;; https://www.emacswiki.org/emacs/CustomThemes
(let ((basedir "~/.emacs.d/themes/"))
      (dolist (f (directory-files basedir))
        (if (and (not (or (equal f ".") (equal f "..")))
                 (file-directory-p (concat basedir f)))
            (add-to-list 'custom-theme-load-path (concat basedir f)))))

(load-theme 'material t)

(provide 'init-themes)
