;;; funcs.el --- gf-org layer functions file for Spacemacs.
;;; Commentary:
;;; Code:

(defun gf-org/reload ()
  "Reload the org file for the current month - useful for a long
running emacs instance."
  (interactive)
  (setq gf/current-month-notes-last-visited nil)
  ;; Agenda files are only used for searching - my notes are designed to
  ;; work without scheduling, tags etc
  (setq org-agenda-files (append
                          (file-expand-wildcards (concat org-directory "dates/*.org"))
                          (file-expand-wildcards (concat org-directory "topics/*.org"))
                          (file-expand-wildcards (concat org-directory "topics/*/*.org"))))
  (setq org-default-notes-file
        (concat org-directory "dates/"
                (downcase (format-time-string "%Y-%B.org")))))

(defun gf-org/find-current-month-notes-file ()
  "Find the org file for the current month"
  (interactive)
  (setq gf/current-month-notes-last-visited (format-time-string "%D"))
  (find-file org-default-notes-file))

;;; funcs.el ends here
