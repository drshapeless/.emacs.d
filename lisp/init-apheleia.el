;;; init-apheleia.el --- Format code -*- lexical-binding: t -*-

;;; Commentary:

;; This is an alternative to format-all.

;;; Code:

(straight-use-package 'apheleia)

(add-to-list 'apheleia-mode-alist
             '(swift-mode . swift-format))

(add-to-list 'apheleia-formatters
             '(swift-mode "swift-format"))

(apheleia-global-mode 1)

(provide 'init-apheleia)
;;; init-apheleia.el ends here