;;; init-orderless.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Orderless completion.

;;; Code:

(elpaca
    orderless
  (require 'orderless)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(provide 'init-orderless)
;;; init-orderless.el ends here
