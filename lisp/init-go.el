;;; init-go.el --- Major mode for golang -*- lexical-binding: t -*-

;;; Commentary:

;; The code is mainly from https://github.com/golang/tools/blob/master/gopls/doc/emacs.md.

;; Don't ever use space over tab in golang, golang wants tab.

;;; Code:

(straight-use-package 'go-mode)
(require 'go-mode)

(provide 'init-go)
;;; init-go.el ends here
