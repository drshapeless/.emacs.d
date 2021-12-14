;;; init-tramp.el --- Tramp config. -*- lexical-binding: t -*-

;;; Commentary:

;; Tramp config.

;;; Code:

;; The default git version from straight.el is handicapped.
(leaf tramp
  :straight nil
  ;; :straight (tramp :type git :host nil :repo "https://git.savannah.gnu.org/git/tramp.git")
  :require t
  :config
  ;; This is for remote host to recognize tramp as a unique type of
  ;; shell, useful for remote zsh, e.g. macOS. This should always work
  ;; with .zshrc modification.
  (setq tramp-terminal-type "tramp"))

;; (require 'tramp)
;; (setq tramp-terminal-type "tramp")

(provide 'init-tramp)
;;; init-tramp.el ends here
