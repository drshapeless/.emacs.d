;;; init-aggressive-indent.el --- INDENT! -*- lexical-binding: t -*-

;;; Commentary:

;; Indent automatically. Do not enable by default.

;;; Code:

(straight-use-package 'aggressive-indent)
(require 'aggressive-indent)
;; (global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'dart-mode)
(add-to-list 'aggressive-indent-excluded-modes 'rustic-mode)
(add-to-list 'aggressive-indent-excluded-modes 'svelte-mode)
(add-hook 'nginx-mode-hook #'aggressive-indent-mode)
;; Useful in future.
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; The variable aggressive-indent-dont-indent-if lets you customize
;; when you don't want indentation to happen. For instance, if you
;; think it's annoying that lines jump around in c++-mode because you
;; haven't typed the ; yet, you could add the following clause:
(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                           (thing-at-point 'line)))))

(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                           (thing-at-point 'line)))))

(provide 'init-aggressive-indent)
;;; init-aggressive-indent.el ends here
