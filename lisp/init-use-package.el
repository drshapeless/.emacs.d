;;; init-use-package.el ---
;;; Commentary:

;; use-package configurations.

;;; Code:

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'init-use-package)
;;; init-use-package.el ends here
