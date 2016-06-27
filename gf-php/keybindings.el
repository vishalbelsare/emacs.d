(spacemacs/set-leader-keys
  "oi" 'gf-php/insert-use-class
  "oI" 'gf-php/insert-class)

(spacemacs/set-leader-keys-for-major-mode 'php-mode "=" 'gf-php/cleanup-style)
