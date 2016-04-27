(defun gf-yasnippet/buffer-file-name-body ()
  (if (buffer-file-name)
      (first (split-string (file-name-nondirectory (buffer-file-name)) "\\."))))

(defun gf-yasnippet/filename-extension (filename)
  "Get the extension from a file path."
  (car (last (s-split "\\." filename))))

(defun gf-yasnippet/in-file-type (extension)
  "Return t if the extension of the current buffer file matches
EXTENSION. Only the last extension of the file is considered."
  (equal extension (gf-yasnippet/filename-extension (buffer-file-name))))
