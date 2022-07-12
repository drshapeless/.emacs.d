;;; init-sly.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; A better slime mode.

;;; Code:

(straight-use-package 'sly)
(require 'sly)
(setq sly-command-switch-to-existing-lisp 'always)
(keymap-global-set "C-c l" #'sly)

;; Always use sbcl.
(setq inferior-lisp-program "sbcl")

(add-hook 'sly-mode-hook
          (lambda ()
            (unless (sly-connected-p)
              (save-excursion (sly)))))

(provide 'init-sly)
;;; init-sly.el ends here
