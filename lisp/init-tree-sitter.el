;;; init-tree-sitter.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf tree-sitter
  :require t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(leaf tree-sitter-langs
  :require t)

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
