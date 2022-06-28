;;; init-glsl.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Shader.

;;; Code:

(leaf glsl-mode
  :require t
  ;; This is for bgfx shader, they are very similar.
  :mode (("\\.sc\\'" "\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'" "\\.vs\\'" "\\.fs\\'") . glsl-mode))

(provide 'init-glsl)
;;; init-glsl.el ends here
