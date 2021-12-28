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

;; https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh
;; Speed up tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose 1)


;; (require 'tramp)
;; (setq tramp-terminal-type "tramp")

(provide 'init-tramp)
;;; init-tramp.el ends here
