;;; init-c3.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(elpaca
    (c3-ts-mode :host github :repo "https://github.com/c3lang/c3-ts-mode")
  (require 'c3-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.c3\\'" . c3-ts-mode))
  (add-to-list 'treesit-language-source-alist
               '(c3 "https://github.com/c3lang/tree-sitter-c3"))
  (setq c3-ts-mode-indent-offset 4)
  (add-hook 'c3-ts-mode-hook #'smart-dash-mode)
  (add-hook 'c3-ts-mode-hook #'shapeless-c-arrow-mode))



(provide 'init-c3)
;;; init-c3.el ends here
