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

(defun gf/php-in-template-p ()
  "Return t if the current php file looks like a template."
  (and (> (point-max) 5)
       (not (equal (buffer-substring-no-properties 1 6) "<?php"))))

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
