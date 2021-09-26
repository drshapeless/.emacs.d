;;; init-eglot.el --- Eglot configurations.
;;; Commentary:

;; I prefer eglot over lsp-mode, it is lighter, faster, not filled
;; with a lot of useless functions.

;;; Code:

(use-package eglot)
(require 'eglot)
(add-to-list 'eglot-server-programs
             '((c++-mode c-mode objc-mode)
               "/usr/bin/clangd"
               "--header-insertion=never"))

;; Sourcekit-lsp is not very functional in Linux.
;; (add-to-list 'eglot-server-programs '(swift-mode))

(add-to-list 'eglot-server-programs
             '(python-mode . ("pyls")))
;; (add-to-list 'eglot-server-programs
;;              '((js-mode)
;;                "typescript-language-server"
;;                "--stdio"
;;                ))

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'objc-mode-hook 'eglot-ensure)
(add-hook 'swift-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)

(add-hook 'eglot-ensure 'flymake)

(define-key eglot-mode-map (kbd "C-c r") 'eglot-reconnect)

;; Make javascript indent space as 2.
(setq js-indent-level 2)

(setq compile-command "make")

(provide 'init-eglot)
;;; init-eglot.el ends here
