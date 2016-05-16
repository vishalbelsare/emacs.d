(defconst gf-general-packages
  '(
    wgrep-helm
    rainbow-mode
    ;; remove after https://github.com/syl20bnr/spacemacs/pull/5831 is in a release
    request
    scss-mode
    yaml-mode
    ))

(defun gf-general/init-wgrep-helm ()
  (use-package wgrep-helm
    ))

(defun gf-general/post-init-yaml-mode ()
  (add-hook 'yaml-mode-hook (lambda ()
                              (setq yaml-indent-offset 4))))

(defun gf-general/post-init-scss-mode ()
  (setq css-indent-offset 2)
  (add-hook 'scss-mode-hook (lambda ()
                              (rainbow-mode t))))
