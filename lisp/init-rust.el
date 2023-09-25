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

(straight-use-package 'toml-mode)
(require 'toml-mode)
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

;; cargo mode does not seem to be important as many of its features
;; can be found in rustic.

;; (straight-use-package 'cargo)
;; (require 'cargo)
;; (add-hook 'rustic-mode-hook #'cargo-minor-mode)

;; This is a temporary fix for using internal treesit.
(if (treesit-available-p)
    (progn
      (define-derived-mode rustic-mode rust-ts-mode "Rustic"
        "Major mode for Rust code.

\\{rustic-mode-map}"
        :group 'rustic

        (when (bound-and-true-p rustic-cargo-auto-add-missing-dependencies)
          (add-hook 'lsp-after-diagnostics-hook 'rustic-cargo-add-missing-dependencies-hook nil t)))
      (require 'rust-ts-mode)
      ))

(let ((mode '("\\.rs\\'" . rust-ts-mode)))
  (when (member mode auto-mode-alist)
    (setq auto-mode-alist (remove mode auto-mode-alist))))

(setq rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer"))

(setq rustic-ansi-faces ["#000000"
                         "#FF033E"
                         "#2EFF2E"
                         "#FAFA0F"
                         "#0E86D4"
                         "#FF0BAC"
                         "#5CFFFF"
                         "#E8E8E8"])

(provide 'init-rust)
;;; init-rust.el ends here
