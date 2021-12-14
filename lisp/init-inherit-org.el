;;; init-inherit-org.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Make other major modes look like org.

;;; Code:

(leaf inherit-org
  :straight (inherit-org :type git :host github :repo "chenyanming/inherit-org")
  :require t)

(provide 'init-inherit-org)
;;; init-inherit-org.el ends here
