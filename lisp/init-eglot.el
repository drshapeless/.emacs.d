;;; init-eglot.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf eglot
  :require t
  :hook
  ((c-mode-hook c++-mode-hook objc-mode-hook swift-mode-hook python-mode-hook js-mode-hook) . eglot-ensure)
  (eglot-ensure . flymake)
  :bind
  (:eglot-mode-map
   ("C-c r" . elgot-reconnect))
  :config
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("sourcekit-lsp"))))

(setq js-indent-level 2)
(setq compile-command "make")

(provide 'init-eglot)
;;; init-eglot.el ends here
