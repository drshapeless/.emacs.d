;;; init-tree-sitter.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; A better syntax highlighting.

;;; Code:

(straight-use-package 'tree-sitter)
(require 'tree-sitter)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(straight-use-package 'tree-sitter-langs)
(require 'tree-sitter-langs)

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
