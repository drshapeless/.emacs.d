;;; init-auto-package-update.el ---
;;; Commentary:

;; Automatically update packages.

;;; Code:

(use-package auto-package-update)
(setq auto-package-update-at-time "03:00")
(add-hook 'auto-package-update-after-hook
          (lambda () (message "All packages are updated.")))

(provide 'init-auto-package-update)
;;; init-auto-package-update.el ends here
