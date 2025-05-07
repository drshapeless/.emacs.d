;;; init-glsl.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Shader.

;;; Code:

(elpaca
    glsl-mode
  (require 'glsl-mode)
  (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
  (add-hook 'glsl-ts-mode-hook (lambda () (apheleia-mode 0)))
  (add-hook 'glsl-ts-mode-hook #'eglot-format-buffer-on-save)
  )

(provide 'init-glsl)
;;; init-glsl.el ends here
