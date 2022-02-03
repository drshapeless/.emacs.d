;;; init-sly.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; A better slime mode.

;;; Code:

(leaf sly
  :require t)

;; The homebrew version of clisp is broken.
(if *is-a-mac*
    (setq inferior-lisp-program "sbcl")
  (setq inferior-lisp-program "clisp"))

(provide 'init-sly)
;;; init-sly.el ends here
