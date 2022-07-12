;;; init-orderless.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Orderless completion.

;;; Code:

(straight-use-package 'orderless)
(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

(provide 'init-orderless)
;;; init-orderless.el ends here
