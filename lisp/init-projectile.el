;;; init-projectile.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf projectile
  :after ripgrep
  :init
  (projectile-mode +1)
  :bind
  (:projectile-mode-map
   ("C-c p" . projectile-command-map)))

(provide 'init-projectile)
;;; init-projectile.el ends here
