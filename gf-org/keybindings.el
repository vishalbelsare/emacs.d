(spacemacs/set-leader-keys "op" 'gf-org/toggle-switch-to-project-org-file)
(spacemacs/set-leader-keys "om" 'gf-org/find-current-month-notes-file)
(spacemacs/set-leader-keys "oN" 'gf-org/commit-notes)
;; refile over files
(spacemacs/set-leader-keys-for-major-mode 'org-mode "R" 'gf-org/refile-files-first)
;; refile withing the same file
(spacemacs/set-leader-keys-for-major-mode 'org-mode "r" 'org-refile)

