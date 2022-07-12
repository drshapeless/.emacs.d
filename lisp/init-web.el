;;; init-web.el --- Web mode -*- lexical-binding: t -*-

;;; Commentary:

;; Web mode is a mode for editing html templates.

;;; Code:

(straight-use-package 'web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))

(provide 'init-web)
;;; init-web.el ends here
