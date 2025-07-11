;;; init-eldoc-box.el --- Display eldoc in child frame -*- lexical-binding: t -*-

;;; Commentary:

;; This makes eglot much more modern, not sure if that is good or bad.

;;; Code:

(elpaca eldoc-box
  (require 'eldoc-box)

  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode)
  )


(provide 'init-eldoc-box)
;;; init-eldoc-box.el ends here
