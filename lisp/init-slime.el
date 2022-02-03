;;; init-slime.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Superior interaction mode for Emacs.

;;; Code:

(leaf slime
  :require t
  :config
  (setq inferior-lisp-program "clisp"))

(provide 'init-slime)
;;; init-slime.el ends here
