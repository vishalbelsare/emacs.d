(defconst gf-general-packages
  '(
    wgrep-helm
    ;; remove after https://github.com/syl20bnr/spacemacs/pull/5831 is in a release
    request
    yaml-mode
    ))

(defun gf-general/init-wgrep-helm ()
  (use-package wgrep-helm
    ))

(defun gf-general/post-init-yaml-mode ()
  (add-hook 'yaml-mode-hook (lambda ()
                              (setq yaml-indent-offset 4))))
