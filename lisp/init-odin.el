;;; init-odin.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; odin lang

;;; Code:

(elpaca (odin-ts-mode :host github :repo "drshapeless/odin-ts-mode")
  (require 'odin-ts-mode)
  (add-to-list 'treesit-language-source-alist
               '(odin "https://github.com/tree-sitter-grammars/tree-sitter-odin"))
  (add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-ts-mode))
  (if *is-a-linux*
      (setenv "ODIN_ROOT" (concat (getenv "HOME") "/src/Odin")))
  (add-hook 'odin-ts-mode-hook (lambda () (setq-local shapeless-c-arrow-p nil)))
  (add-hook 'odin-ts-mode-hook #'eglot-format-buffer-on-save)
  (keymap-set odin-ts-mode-map "C-c c" #'completion-at-point))

(provide 'init-odin)
;;; init-odin.el ends here
