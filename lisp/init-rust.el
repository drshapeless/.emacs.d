;;; init-rust.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Writing rust on Emacs is just unbearably slow. Auto completion is
;; way too laggy. No human on earth can withstand such an experience.

;;; Code:

;; (leaf rust-mode
;;   :require t)

(leaf rustic
  :require t
  :mode "\\.rs\\'"
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-trigger 'on-save)
  )

(leaf toml-mode
  :mode "\\.toml\\'"
  :require t)

(leaf cargo
  :require t
  :hook ((rustic-mode-hook toml-mode-hook) . cargo-minor-mode))

(provide 'init-rust)
;;; init-rust.el ends here
