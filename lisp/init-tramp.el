;;; init-tramp.el --- Tramp config. -*- lexical-binding: t -*-

;;; Commentary:

;; Tramp config.

;;; Code:

;; The latest git version of tramp is full of crap, fuck.
;; Use the built-in one.
(leaf tramp
  :straight nil
  :require t
  :config
  ;; This is for remote host to recognize tramp as a unique type of
  ;; shell, useful for remote zsh, e.g. macOS. This should always work
  ;; with .zshrc modification.
  (setq tramp-terminal-type "tramp")

  ;; This function is missing in the latest git version of
  ;; tramp. Making tramp absolutely unusable.
  ;;   (put #'tramp-dissect-file-name 'tramp-suppress-trace t)

  ;;   (defun tramp-ensure-dissected-file-name (vec-or-filename)
  ;;     "Return a `tramp-file-name' structure for VEC-OR-FILENAME.

  ;; VEC-OR-FILENAME may be either a string or a `tramp-file-name'.
  ;; If it's not a Tramp filename, return nil."
  ;;     (cond
  ;;      ((tramp-file-name-p vec-or-filename) vec-or-filename)
  ;;      ((tramp-tramp-file-p vec-or-filename)
  ;;       (tramp-dissect-file-name vec-or-filename))))
  )

;; (require 'tramp)
;; (setq tramp-terminal-type "tramp")

(provide 'init-tramp)
;;; init-tramp.el ends here
