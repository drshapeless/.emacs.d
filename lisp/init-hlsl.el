;;; init-hlsl.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; hlsl

;;; Code:

(leaf hlsl-mode
  ;; This file is an local file.
  :straight nil
  :require t
  :mode (("\\.fx\\'" "\\.hlsl\\'") . hlsl-mode))

(provide 'init-hlsl)
;;; init-hlsl.el ends here
