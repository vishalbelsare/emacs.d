;;; packages.el --- gf-org layer packages file for Spacemacs.
;;; Commentary:
;;; Code:

(defconst gf-org-packages
  '(org
    ))

(defun gf-org/post-init-org ()
  "Setup org mode. Notes are split into 3 folders:

/dates/ - contains monthly notes files, e.g. 2016-april.org
/projects/
/topics/ - for topics large enough to have their own file

Notes are grouped by months for automatic archival.
At the start of every month, move over notes/tasks that are still relevant.
"
  (setq org-directory "~/notes/")
  (setq org-listen-read-watch-file (concat org-directory "topics/listen-read-watch.org"))

  (setq org-files (append (file-expand-wildcards (concat org-directory "*/*.org"))
                          (file-expand-wildcards (concat org-directory "*/*/*.org"))))

  (gf-org/reload)
  )

;;; packages.el ends here
