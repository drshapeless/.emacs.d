;;; init-marginalia.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Display more information on the right margin.

;;; Code:

;; Enable richer annotations using the Marginalia package
(elpaca
 marginalia
 (require 'marginalia)
 (marginalia-mode t)
 (setq marginalia-align 'right)

 ;; This is kind of useless.
 (keymap-set minibuffer-local-map "M-A" #'marginalia-cycle))

(provide 'init-marginalia)
;;; init-marginalia.el ends here
