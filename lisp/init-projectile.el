;;; init-projectile.el ---
;;; Commentary:

;; Projectile.

;;; Code:

(use-package projectile
  :diminish
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(provide 'init-projectile)
;;; init-projectile.el ends here
