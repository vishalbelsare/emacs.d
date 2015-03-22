(require 'smartparens-config)

(smartparens-global-strict-mode)

(define-key global-map (kbd "C-c s") 'sp-slurp-hybrid-sexp)

(require 'evil-smartparens)

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(provide 'setup-smartparens)
