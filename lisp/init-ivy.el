;;; init-ivy.el ---
;;; Commentary:

;;

;;; Code:

(use-package ivy)
(require 'ivy)
(ivy-mode t)
(diminish 'ivy-mode)
(setq ivy-extra-directories '("./"))

;; The default is 10. You may want to set it smaller in laptop.
(setq ivy-height 10)

(use-package ivy-rich)
(require 'ivy-rich)
(ivy-rich-mode 1)

(provide 'init-ivy)
;;; init-ivy.el ends here
