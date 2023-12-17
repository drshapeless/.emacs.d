;;; init-go.el --- Major mode for golang -*- lexical-binding: t -*-

;;; Commentary:

;; The code is mainly from https://github.com/golang/tools/blob/master/gopls/doc/emacs.md.

;; Don't ever use space over tab in golang, golang wants tab.

;;; Code:

(straight-use-package 'go-mode)
(require 'go-mode)
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'go-ts-mode-hook (lambda () (setq tab-width 4)))
(setq go-ts-mode-indent-offset 4)

(defun drsl/go-db ()
  "Insert the snake_case version of current field "
  (interactive)
  (let ((word (string-inflection-underscore-function (current-word))))
    (end-of-line)
    (insert " `db:\"" word "\"`")))

(define-derived-mode templ-mode go-mode "Templ Mode" "Major mode for templ file"
  )
(font-lock-add-keywords 'templ-mode '(("templ " . 'font-lock-keyword-face)))

(add-to-list 'auto-mode-alist '("\\.templ\\'" . templ-mode))

(provide 'init-go)
;;; init-go.el ends here
