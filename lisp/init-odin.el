;;; init-odin.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; odin lang

;;; Code:

(elpaca (odin-ts-mode :host github :repo "Sampie159/odin-ts-mode")
  (add-to-list 'treesit-language-source-alist
               '(odin "https://github.com/tree-sitter-grammars/tree-sitter-odin"))
  (add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-ts-mode)))

(provide 'init-odin)
;;; init-odin.el ends here
