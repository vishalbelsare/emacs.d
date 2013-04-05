(require 'elscreen)
(elscreen-start)

;; Share emacs
(require 'server)
(unless (server-running-p)
  (server-start))

;;Allows launching from chrome textareas
(require 'edit-server nil t)
(unless (process-status "edit-server")
  (setq edit-server-new-frame t)
  (edit-server-start))

;; Flyspell
(setq flyspell-issue-message-flag nil)

(dolist (hook '(org-mode-hook magit-log-edit-mode-hook))
  (add-hook hook (lambda ()
				   (flyspell-mode 1)
				   (auto-fill-mode 1)
				   )))

;;; Ido
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere t)
(setq org-completion-use-ido t)
(setq ido-max-directory-size 100000)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq kill-buffer-query-functions
	  (remq 'process-kill-buffer-query-function
			kill-buffer-query-functions))
(setq ido-ignore-buffers (append '("^\*Completions\*" "^\*Help\*" "^\*magit-process\*" "^\*Compile-Log\*" "^\*vc-diff\*") ido-ignore-buffers))
(setq ido-auto-merge-delay-time 99999)
(add-hook 'ido-setup-hook (lambda ()
     (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)))

;; Ido for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; General modes
(delete-selection-mode t)
(recentf-mode 1)
(show-paren-mode t)
(column-number-mode t)
(tooltip-mode -1)
;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)
(require 'ace-jump-mode)

;;; General settings
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode t)
(setq auto-save-default nil)
(setq hl-line-sticky-flag 1)
(setq ido-default-buffer-method 'selected-window)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq x-select-enable-clipboard t)
(setq undo-tree-visualizer-timestamps 1)

;;; Enable normally disabled functions
(put 'ido-exit-minibuffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Automatically create directories when creating a file
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
	(let ((dir (file-name-directory filename)))
	  (unless (file-exists-p dir)
		(make-directory dir)))))

;; Remove any trailing whitespace on buffer write
(define-minor-mode remove-trailing-whitespace-mode
  "Toggle remove trailing whitespace on save.
When enabled trailing whitespace is removed before saving."
  :init-value nil
  :global t
  :lighter " W"

  (if remove-trailing-whitespace-mode
	  (add-hook 'before-save-hook 'delete-trailing-whitespace)
	(remove-hook 'before-save-hook 'delete-trailing-whitespace)))

(remove-trailing-whitespace-mode t)


(provide 'setup-general)
