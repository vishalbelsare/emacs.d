(use-package js2-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  (setq js2-mode-hook (lambda ()
			(flycheck-mode -1)))

  (defun gf/js2-mode-toggle-indent-offset ()
    "Switch between 2 and 4 spaces for javascript indentation"
    (interactive)
    (if (eq js2-basic-offset 2)
	(setq js2-basic-offset 4)
      (setq js2-basic-offset 2))
    (message (format "Javascript indendation is now %s spaces" js2-basic-offset)))

  (setq js2-basic-offset 2)

  )

(use-package js2-refactor :ensure t
  :after js2-mode)

(use-package js-comint :ensure t
  :config
  (setq inferior-js-program-command "env NODE_NO_READLINE=1 node")) 

(provide 'setup-js)
