;;; init-jai.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Jai programming language

;;; Code:

(elpaca
    (jai-ts-mode :host github :repo "cpoile/jai-ts-mode")
  (require 'jai-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-ts-mode))
  (add-hook 'jai-ts-mode-hook (lambda () (setq-local corfu-auto nil)))
  (modify-syntax-entry ?_ "_" jai-ts-mode--syntax-table))

(provide 'init-jai)
;;; init-jai.el ends here
