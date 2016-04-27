(defconst gf-yasnippet-packages
  '(yasnippet
    ))

(defun gf-yasnippet/post-init-yasnippet ()
  (use-package yasnippet
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "Y" "yasnippet")
      (spacemacs/set-leader-keys "Yy" #'yas-insert-snippet)
      (spacemacs/set-leader-keys "Yr" #'yas-reload-all)

      (setq yas-snippet-dirs '("~/.spacemacs-layers/snippets")))
    :config
    (progn
      )))
