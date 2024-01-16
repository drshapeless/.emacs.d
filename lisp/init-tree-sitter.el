;;; init-tree-sitter.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; A better syntax highlighting.

;;; Code:

;; (straight-use-package 'tree-sitter)
;; (require 'tree-sitter)

;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; (straight-use-package 'tree-sitter-langs)
;; (require 'tree-sitter-langs)

(setq treesit-language-source-alist
      '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (js . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        ))

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (go-mode         . go-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (conf-toml-mode  . toml-ts-mode)
        (css-mode        . css-ts-mode)
        (js-mode         . js-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-json-mode    . json-ts-mode)
        (python-mode     . python-ts-mode)
        (sh-mode         . bash-ts-mode)
        (rust-mode       . rust-ts-mode)
        (typescript-mode . typescript-ts-mode)))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
