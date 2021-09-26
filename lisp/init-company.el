;;; init-company.el ---
;;; Commentary:

;;

;;; Code:

(use-package company)
(require 'company)
(diminish 'company-mode)
(global-company-mode t)
(setq company-idle-delay 0)
(setq company-echo-delay 0)
(setq company-dabbrev-downcase nil)	; Enable case sensitive.
(setq company-minimum-prefix-length 1)

(provide 'init-company)
;;; init-company.el ends here
