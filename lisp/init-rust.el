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
  ;; (add-hook 'eglot-managed-mode-hook (lambda () (if (eq major-mode 'rustic-mode)
  ;;                                                   (flymake-mode -1))))
  )

(leaf toml-mode
  :mode "\\.toml\\'"
  :require t)

(leaf cargo
  :require t
  :hook ((rustic-mode-hook toml-mode-hook) . cargo-minor-mode))

;; (add-hook 'rust-mode-hook
;;           (lambda () (setq indent-tabs-mode nil)))

;; (setq rust-format-on-save t)

;; (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

(provide 'init-rust)
;;; init-rust.el ends here
