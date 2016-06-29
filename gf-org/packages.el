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
  (setq org-directory (expand-file-name "~/notes/"))
  (setq gf-org/projects-dir (concat org-directory  "projects"))
  (setq gf-org/listen-read-watch-file (concat org-directory "topics/listen-read-watch.org"))
  (setq gf-org/refile-candidates
        (append (file-expand-wildcards (concat org-directory "*/*.org"))
                (file-expand-wildcards (concat org-directory "*/*/*.org"))))
  (use-package org
    :config
    (progn
      (gf-org/reload)
      ;; Split up the search string on whitespace
      (setq org-agenda-search-view-always-boolean t)

      (setq org-confirm-babel-evaluate nil)
      (setq org-src-fontify-natively t)
      (setq org-src-tab-acts-natively t)
      (setq org-startup-indented nil)
      (setq org-log-done t)
      ;; Make it impossible to complete a task if subtasks are not done
      (setq org-enforce-todo-dependencies t)
      (setq org-use-fast-todo-selection t)

      (setq org-refile-targets
            '((nil :maxlevel . 2)))

      (setq org-todo-keywords
            '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w)" "|" "CANCELLED(c)")))

      (setq org-todo-keyword-faces
            (quote (("TODO" :foreground "#dc322f" :weight bold)
                    ("DONE" :foreground "forest green" :weight bold :strike-through t)
                    ("WAITING" :foreground "#89BDFF" :weight bold))))

      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
               "* TODO %?" :prepend t)
              ("n" "Note" entry (file+headline org-default-notes-file "Notes")
               "* %?")
              ;; ("T" "Project Todo" entry (file+headline gf/current-project-file "Tasks")
              ;;  "* TODO %?" :prepend t)
              ;; ("N" "Project Note" entry (file+headline gf/current-project-file "Notes")
              ;;  "* %?")
              ("l" "Listen" entry (file+headline gf-org/listen-read-watch-file "Listen")
               "* %?")
              ("r" "Read" entry (file+headline gf-org/listen-read-watch-file "Read")
               "* %?")
              ("w" "Watch" entry (file+headline gf-org/listen-read-watch-file "Watch")
               "* %?")
              ("p" "Play" entry (file+headline gf-org/listen-read-watch-file "Play")
               "* %?")
              ))

      (add-hook 'org-mode-hook 'auto-fill-mode))))

;;; packages.el ends here
