;;; init-zig.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; zig programming language

;;; Code:

(use-package zig-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/zig-ts-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))
  (add-to-list 'treesit-language-source-alist
               '(zig "https://github.com/maxxnino/tree-sitter-zig")))

(provide 'init-zig)
;;; init-zig.el ends here
