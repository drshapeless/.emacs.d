;;; init-go.el --- Major mode for golang -*- lexical-binding: t -*-

;;; Commentary:

;; The code is mainly from https://github.com/golang/tools/blob/master/gopls/doc/emacs.md.

;;; Code:

(leaf go-mode
  :require t
  :hook
  (go-mode-hook . drsl/go-mode-hook))

;; Don't ever use space over tab in golang, golang wants tab.
(defun drsl/go-mode-hook ()
  (setq tab-width 2))

(provide 'init-go)
;;; init-go.el ends here
