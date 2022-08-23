;;; init-typescript.el --- TypeScript support -*- lexical-binding: t -*-

;;; Commentary:

;; I don't really like JavaScript, and TypeScript pretends that the
;; problem of JavaScript is because of missing a type system.

;;; Code:

(straight-use-package 'typescript-mode)
(require 'typescript-mode)

;; This line should not be here. But the js mode is built-in, and I
;; don't bother to create a dummy file for that.
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-mode))

(provide 'init-typescript)
;;; init-typescript.el ends here
