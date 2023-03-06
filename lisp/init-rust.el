;;; init-rust.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Writing rust in Emacs with a large library as dependency is
;; unbearabily slow.

;;; Code:

(straight-use-package 'rustic)
(require 'rustic)
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
(setq rustic-lsp-client 'eglot)
(setq rustic-format-trigger 'on-save)

;; This is a temporary fix for using internal treesit.
(if (and (not *is-older-emacs*) (treesit-available-p))
    (define-derived-mode rustic-mode rust-ts-mode "Rustic"
      "Major mode for Rust code.

\\{rustic-mode-map}"
      :group 'rustic

      (when (bound-and-true-p rustic-cargo-auto-add-missing-dependencies)
        (add-hook 'lsp-after-diagnostics-hook 'rustic-cargo-add-missing-dependencies-hook nil t))))

(straight-use-package 'toml-mode)
(require 'toml-mode)
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

(straight-use-package 'cargo)
(require 'cargo)
(add-hook 'rustic-mode-hook #'cargo-minor-mode)
(add-hook 'toml-mode-hook #'cargo-minor-mode)

(provide 'init-rust)
;;; init-rust.el ends here
