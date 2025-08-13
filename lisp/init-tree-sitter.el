;;; init-tree-sitter.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; A better syntax highlighting.

;;; Code:

(require 'treesit)
(add-to-list 'treesit-language-source-alist
             '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc"))
(add-to-list 'treesit-language-source-alist
             '(jai "https://github.com/constantitus/tree-sitter-jai"))

(setq-default treesit-font-lock-level 4)
;; (setq treesit-language-source-alist
;;       '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
;;         (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
;;         (go . ("https://github.com/tree-sitter/tree-sitter-go"))
;;         (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
;;         (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
;;         (python . ("https://github.com/tree-sitter/tree-sitter-python"))
;;         (templ . ("https://github.com/vrischmann/tree-sitter-templ"))
;;         (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
;;         (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
;;         ))

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (c-or-c++-mode   . c-or-c++-ts-mode)
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
