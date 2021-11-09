;;; init-orderless.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(provide 'init-orderless)
;;; init-orderless.el ends here
