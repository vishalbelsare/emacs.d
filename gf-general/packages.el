(defconst gf-general-packages
  '(
    wgrep-helm
    ;; remove after https://github.com/syl20bnr/spacemacs/pull/5831 is in a release
    request
    ))

(defun gf-general/init-wgrep-helm ()
  (use-package wgrep-helm
    ))
