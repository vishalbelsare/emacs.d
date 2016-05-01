(defun gf-php/current-file-namespace ()
  "Get a suitable namespace for the current file."
  (interactive)
  (if (s-contains? "src" (buffer-file-name))
      (gf-php/namespace-from-path buffer-file-name "src")

    (if (s-contains? "tests" (buffer-file-name))
        (gf-php/namespace-from-path buffer-file-name "tests")

      (if (s-contains? "app" (buffer-file-name))
          (gf-php/namespace-from-path buffer-file-name "app")))))

(defun gf-php/namespace-from-path (path substr)
  "Extract a namespace from a path name that contains `substr`."
  (let ((namespace-file
         (s-right (- (length path)
                     (s-index-of substr path)
                     (+ 1 (length substr)))
                  path)))
    (s-join "\\" (butlast (s-split "/" namespace-file)))))

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
