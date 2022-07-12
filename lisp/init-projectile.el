;;; init-projectile.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; This package is no longer useful. Most of its functionalities can
;; be replaced with the built-in project.el

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
