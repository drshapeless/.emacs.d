;;; init-atomic-chrome.el --- Browser integration -*- lexical-binding: t -*-

;;; Commentary:

;; atomic chrome is a cursed name, I am using it in Firefox.

;;; Code:

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

(provide 'init-atomic-chrome)
;;; init-atomic-chrome.el ends here
