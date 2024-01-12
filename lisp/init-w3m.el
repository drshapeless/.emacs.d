;;; init-w3m.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; w3m is a great browser, but currently I am facing a duckduckgo
;; encoding issue.

;;; Code:

(straight-use-package 'w3m)
(require 'w3m nil t)
;; (add-hook 'w3m-fontify-before-hook #'inherit-org-w3m-headline-fontify)
;; (add-hook 'w3m-fontify-after-hook #'inherit-org-mode)
(setq w3m-use-cookies nil
      ;; There is something encoding error when using duckduckgo.
      ;; w3m-search-default-engine "duckduckgo"
      )

(keymap-global-set "C-c j" #'w3m)

(provide 'init-w3m)
;;; init-w3m.el ends here
