;; Org mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(mapcar (lambda (state)
      (evil-declare-key state org-mode-map
        (kbd "M-l") 'org-metaright
        (kbd "M-h") 'org-metaleft
        (kbd "M-k") 'org-metaup
        (kbd "M-j") 'org-metadown
        (kbd "M-L") 'org-shiftmetaright
        (kbd "M-H") 'org-shiftmetaleft
        (kbd "M-K") 'org-shiftmetaup
        (kbd "M-J") 'org-shiftmetadown))
    '(normal insert))




;; quick hotkey for searching notes
(define-key global-map (kbd "C-c n") 'org-search-view)

(defvar gf/current-month-notes-last-visited nil
  "The last date the org file for the current month was opened.")

(defun gf/check-current-month-notes-reminder ()
  "Show a reminder message if the current notes file hasn't been visited today."
  (if (not (equal gf/current-month-notes-last-visited (format-time-string "%D")))
      (message (format "Check your notes for today, %s" (format-time-string "%A %e of %B")))))

(add-hook 'find-file-hook 'gf/check-current-month-notes-reminder)

(define-key global-map (kbd "C-c C-n") (lambda ()
                                       (interactive)
                                       (projectile-find-file-in-directory org-directory)))

(evil-declare-key 'normal org-mode-map (kbd "C-t") 'org-shiftright)
(evil-declare-key 'insert org-mode-map (kbd "C-t") 'org-shiftright)
(evil-declare-key 'normal org-mode-map (kbd "C-S-t") 'org-shiftleft)
(evil-declare-key 'insert org-mode-map (kbd "C-S-t") 'org-shiftleft)

(evil-declare-key 'normal org-mode-map (kbd "gn") 'gf/org-go-to-next-task)
(define-key org-mode-map (kbd "C-c t") 'org-todo)

(evil-declare-key 'insert org-mode-map (kbd "M-<return>") (lambda()
                                (interactive)
                                (evil-append-line 1)
                                (org-meta-return)
                                ))
(evil-declare-key 'normal org-mode-map (kbd "M-<return>") (lambda()
                                (interactive)
                                (evil-append-line 1)
                                (org-meta-return)
                                ))
(evil-declare-key 'normal org-mode-map (kbd "<return>") 'org-open-at-point)
(evil-declare-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

(define-key org-mode-map (kbd "C-S-<up>") 'delete-other-windows)
(define-key org-mode-map (kbd "C-j") 'evil-window-down)
(define-key org-mode-map (kbd "C-k") 'evil-window-up)
(define-key global-map (kbd "C-c a") 'org-agenda)

(define-key global-map (kbd "C-c l") 'org-store-link)
;;; to insert the link into an org mode buffer, use C-c C-l

;; Vim style navigation
(define-key org-mode-map (kbd "C-c h") 'outline-up-heading)
(define-key org-mode-map (kbd "C-c j") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "C-c k") 'outline-previous-visible-heading)
(define-key org-mode-map (kbd "C-c g") 'gf/org-end-of-section)
(define-key org-mode-map (kbd "C-c J") 'org-forward-heading-same-level)
(define-key org-mode-map (kbd "C-c K") 'org-backward-heading-same-level)

(defun gf/org-end-of-section ()
  "Move to the last line of the current section."
  (interactive)
  (re-search-backward "^\* ")
  (org-forward-element 1)
  (previous-line 1))

(defun gf/evil-org-beginning-of-line ()
  "Move to the beginning of the line in an org-mode file, ignoring
TODO keywords, stars and list indicators."
  (interactive)
  (beginning-of-line)
  (if (looking-at-p " ") (evil-forward-word-begin))
  (if (looking-at-p "*") (evil-forward-word-begin))
  (if (looking-at-p "TODO\\|DONE\\|NEXT\\|WAITING") (evil-forward-word-begin)))

(defun gf/org-go-to-next-task ()
  "Go to the first org item in the buffer tagged as `NEXT`."
  (interactive)
  (beginning-of-buffer)
  (re-search-forward "^\\*+ NEXT")
  (gf/evil-org-beginning-of-line))

(evil-declare-key 'normal org-mode-map "^" 'gf/evil-org-beginning-of-line)
(evil-declare-key 'normal org-mode-map "I"
  (lambda ()
    (interactive)
    (gf/evil-org-beginning-of-line)
    (evil-insert 1)
    ))

(evil-declare-key 'normal org-mode-map ",N" 'org-narrow-to-subtree)

(evil-declare-key 'normal org-mode-map (kbd "M-i") 'org-display-inline-images)
(evil-declare-key 'normal org-mode-map (kbd "M-I") 'org-remove-inline-images)

;; Behaviour for capturing notes using make-capture-frame
(defadvice org-capture-finalize
  (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
  (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-switch-to-buffer-other-window
  (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

;;babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (js . t)
   (lilypond . t)
   (haskell . t)
   (python . t)
   (sh . t)
   ))

(provide 'setup-org)
