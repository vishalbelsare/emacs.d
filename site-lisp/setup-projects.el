(require 'projectile)
(projectile-global-mode)

(setq helm-projectile-sources-list '(helm-source-projectile-buffers-list helm-source-projectile-files-list))

(defun gf/create-project-branch-from-org-heading ()
  "Create a git feature branch for the current org heading. The project is guessed from the current org file."

  )

(provide 'setup-projects)
