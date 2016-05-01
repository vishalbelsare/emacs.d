(require 'php-mode)
(require 'web-mode)
(require 's)
(require 'php-auto-yasnippets)

;; Install refactor tool with `composer global require qafoolabs/php-refactoring-browser`
(require 'php-refactor-mode)
(setq php-refactor-command (expand-file-name "~/.composer/vendor/bin/refactor"))
(add-hook 'php-mode-hook 'php-refactor-mode)

(setq web-mode-disable-auto-pairing nil)
(setq web-mode-disable-css-colorization nil)


(defun gf/toggle-php-web-mode ()
  "Switch between php-mode and web-mode for the current buffer."
  (interactive)
  (if (equal (symbol-name (buffer-local-value 'major-mode (current-buffer))) "web-mode")
      (php-mode)
    (web-mode)
    ))

(defun test-this-or-related-php-file ()
  "Run test-case-run on the current buffer, or the related test case
file if open."
  (interactive)
  (save-buffer)
  (if (or test-case-mode (test-case-mode))
      (test-case-run)
    (let ((b (concat
              (car (split-string (buffer-name) "\\."))
              "Test."
              (car (cdr (split-string (buffer-name) "\\."))))))
      (if (get-buffer b)
          (progn
            (switch-to-buffer-other-window b)
            (save-buffer)
            (test-case-run)
            (other-window 1))
        (message (format "buffer not found: %s" b))))))

(defun gf/evil-open-below-docblock (count)
  "Same as `evil-open-below`, but insert * if in a docblock."
  (interactive "p")
  (evil-first-non-blank)
  (if (looking-at-p "*")
      (progn
        (evil-open-below count)
        (insert "* "))
    (evil-open-below count)))

(evil-declare-key 'normal php-mode-map "o" 'gf/evil-open-below-docblock)

(defun gf/php-cleanup-style ()
  "Cleanup the style of the current php file with php-cs-fixer."
  (interactive)
  (save-buffer)
  (shell-command (concat "php-cs-fixer fix " (buffer-file-name)))
  (let ((point (point)))
    (revert-buffer t t)
    (goto-char point)))

(defun gf/php-in-template-p ()
  "Return t if the current php file looks like a template."
  (and (> (point-max) 5)
       (not (equal (buffer-substring-no-properties 1 6) "<?php"))))

(defun gf/php-insert-symfony-service ()
  "Insert a service name for the current symfony project."
  (interactive)
  (let ((service (helm-comp-read
                  "Service: "
                  (gf/helm-candidates-from-command "php bin/console debug:container | sed -E 's/^ +//g' | cut -d ' ' -f 1")
                  :must-match t
                  )))
    (insert service)))

(defun gf/php-insert-neptune-service ()
  "Insert a service name for the current neptune php project."
  (interactive)
  (let ((service (helm-comp-read
                  "Service: "
                  (gf/helm-candidates-from-command "php neptune service:list -N | tail -n +2")
                  :must-match t
                  )))
    (insert service)))

(defun gf/php-insert-service ()
  "Insert a service name for the current php project"
  (interactive)
  (if (gf/php-in-symfony-project-p)
      (gf/php-insert-symfony-service)
    (if (gf/php-in-neptune-project-p)
        (gf/php-insert-neptune-service)
      (message "Not in a symfony or neptune project."))))

(defun gf/php-insert-symfony-route ()
  "Insert a route name for the current symfony project."
  (interactive)
  (let ((candidate (helm-comp-read
                  "Route: "
                  (gf/helm-candidates-from-command "php bin/console debug:router | sed -E 's/^ +//g' |  cut -d ' ' -f 1 | tail -n +3")
                  :must-match t
                  )))
    (insert candidate)))

(defun gf/helm-candidates-from-command (command)
  "Get helm candidates from running a command in the projectile root."
  (interactive)
  (split-string (shell-command-to-string
                 (concat "cd " (projectile-project-root) " && " command)) "\n" t))

(defun gf/php-in-symfony-project-p ()
  "Return t if the current projectile project is a symfony project."
  (or
   (file-exists-p (concat (projectile-project-root) "app/console"))
   (file-exists-p (concat (projectile-project-root) "application/app/console"))))

(defun gf/php-in-neptune-project-p ()
  "Return t if the current projectile project is a neptune php project."
  (file-exists-p (concat (projectile-project-root) "neptune")))

;; Use M-j to open a line below when in insert mode

(setq php-auto-yasnippet-php-program (concat plugins-dir "/php-auto-yasnippets/Create-PHP-YASnippet.php"))
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

(evil-declare-key 'normal php-mode-map ",z" 'gf/toggle-php-web-mode)
(evil-declare-key 'normal web-mode-map ",z" 'gf/toggle-php-web-mode)
(define-key php-mode-map (kbd "C-c i") 'gf/php-insert-use-class)
(define-key php-mode-map (kbd "C-c I") 'gf/php-insert-class)
(define-key php-mode-map (kbd "C-c s") 'gf/php-insert-service)

(define-key php-mode-map (kbd "M-q") 'gf/quit-other-window)

(provide 'setup-php)
