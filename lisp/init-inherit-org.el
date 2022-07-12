;;; init-inherit-org.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Make other major modes look like org.

;;; Code:

(straight-use-package '(inherit-org :type git :host github :repo "chenyanming/inherit-org"))
(require 'inherit-org)

(provide 'init-inherit-org)
;;; init-inherit-org.el ends here
