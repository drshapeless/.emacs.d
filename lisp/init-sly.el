;;; init-sly.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; A better slime mode.

;;; Code:

(leaf sly
  :require t
  :config
  (setq inferior-lisp-program "clisp"))

(provide 'init-sly)
;;; init-sly.el ends here
