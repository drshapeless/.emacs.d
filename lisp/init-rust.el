;;; init-rust.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Writing rust in Emacs with a large library as dependency is
;; unbearabily slow.

;;; Code:

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after (rust-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-trigger 'on-save)
  (setq rustic-analyzer-command '("rust-analyzer"))

  (setq rustic-ansi-faces ["#000000"
                           "#FF033E"
                           "#2EFF2E"
                           "#FAFA0F"
                           "#0E86D4"
                           "#FF0BAC"
                           "#5CFFFF"
                           "#E8E8E8"])
  (add-hook 'rustic-mode-hook #'shapeless-prog-mode)
  (add-hook 'rustic-mode-hook (lambda () (setq-local shapeless-c-arrow-p nil))))

(elpaca
    toml-mode
  (require 'toml-mode)
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode)))

(provide 'init-rust)
;;; init-rust.el ends here
