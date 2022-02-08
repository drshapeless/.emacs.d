;;; init-sly.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; A better slime mode.

;;; Code:

(leaf sly
  :require t
  :config
  (setq sly-command-switch-to-existing-lisp 'always)
  :bind
  ("C-c l" . sly))

;; The homebrew version of clisp is broken.
;; Using clisp is for learning only.
(if *is-a-mac*
    (setq inferior-lisp-program "sbcl")
  (setq inferior-lisp-program "clisp"))

(add-hook 'sly-mode-hook
          (lambda ()
            (unless (sly-connected-p)
              (save-excursion (sly)))))

(provide 'init-sly)
;;; init-sly.el ends here
