;;; init-tramp.el --- Tramp config. -*- lexical-binding: t -*-

;;; Commentary:

;; Use the built-in tramp.

;;; Code:

(require 'tramp)
;; This is for remote host to recognize tramp as a unique type of
;; shell, useful for remote zsh, e.g. macOS. This should always work
;; with .zshrc modification.
(setq tramp-terminal-type "tramp")

;; https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh
;; Speed up tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose 1)

(provide 'init-tramp)
;;; init-tramp.el ends here
