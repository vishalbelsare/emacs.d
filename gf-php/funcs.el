(defun gf-php/current-file-namespace ()
  "Get a suitable namespace for the current file."
  (gf-php/file-namespace buffer-file-name))

(defun gf-php/file-namespace (file)
  "Get a suitable namespace for `FILE`."
  (shell-command-to-string
   (concat "~/.spacemacs-layers/bin/php_resolve_namespace.php " file)))

(defun gf-php/go-to-namespace ()
  "Go to the namespace declaration at the top of this file."
  (interactive)
  (beginning-of-buffer)
  (if (not (re-search-forward "^namespace" nil t))
      (error "Namespace declaration not found.")
    (beginning-of-line)))

(defun gf-php/go-to-last-use-statement ()
  "Go to the last use statement at the top of this file."
  (interactive)
  (end-of-buffer)
  (if (not (re-search-backward "^use" nil t))
      (gf-php/go-to-namespace))
  (recenter))

(defun gf-php/class-candidates ()
  "Get a list of available PHP classes in the current projectile project."
  (interactive)
  (split-string (shell-command-to-string
                 (concat "~/.spacemacs-layers/bin/php_class_finder.php " (projectile-project-root))) "\n" t))

(defun gf-php/refresh-class-candidates ()
  "Refresh the list of available PHP classes in the current projectile project."
  (interactive)
  (start-process-shell-command "php-class-candidates" nil (concat "~/.spacemacs-layers/bin/php_class_finder.php " (projectile-project-root) " refresh"))
  (message (format "Refreshing class candidates for %s" (projectile-project-root))))

(defun gf-php/insert-use-class ()
  "Add a class to the use declarations in the current file."
  (interactive)
  (save-excursion
    (let ((class (helm-comp-read
                  "Class: "
                  (gf-php/class-candidates)
                  :must-match t
                  )))
      (gf-php/go-to-last-use-statement)
      (end-of-line)
      (newline)
      (insert (concat "use " class ";")))))

(defun gf-php/insert-class ()
  "Insert a class name in the current projectile project."
  (interactive)
  (let ((class (helm-comp-read
                "Class: "
                (gf-php/class-candidates)
                :must-match t
                )))
    (insert class)))

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

(defun gf-php/cleanup-style ()
  "Cleanup the style of the current php file with php-cs-fixer."
  (interactive)
  (save-buffer)
  (shell-command (concat "php-cs-fixer fix " (buffer-file-name)))
  (let ((point (point)))
    (revert-buffer t t)
    (goto-char point)))
