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
