;;; init-glsl.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Shader.

;;; Code:

(straight-use-package 'glsl-mode)
(require 'glsl-mode)
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))

(provide 'init-glsl)
;;; init-glsl.el ends here
