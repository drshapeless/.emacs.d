;;; init-tramp.el --- Tramp config. -*- lexical-binding: t -*-

;;; Commentary:

;; Tramp config.

;;; Code:

;; This will clone the latest version of tramp.
(leaf tramp
  :require t
  :config
  ;; This is for remote host to recognize tramp as a unique type of
  ;; shell, useful for remote zsh, e.g. macOS. This should always work
  ;; with .zshrc modification.
  (setq tramp-terminal-type "tramp"))


(provide 'init-tramp)
;;; init-tramp.el ends here
