;;; init-company.el --- Company completion config. -*- lexical-binding: t -*-

;;; Commentary:

;; Config for company mode.

;; I am using corfu instead of company.

;;; Code:

(straight-use-package 'company)
(require 'company)
(global-company-mode t)
(setq company-idle-delay 0
      company-echo-delay 0
      company-dabbrev-downcase nil
      company-minimum-prefix-length 1
      company-tooltip-maximum-width 60)

(provide 'init-company)
;;; init-company.el ends here
