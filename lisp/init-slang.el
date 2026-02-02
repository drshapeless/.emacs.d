;;; init-slang.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(elpaca
    (slang-mode :host github :repo "k1ngst0m/slang-mode")
  (require 'slang-mode)
  (require 'slang-lsp)
  ;; (slang-lsp-initialize)
  (add-hook 'slang-mode-hook (lambda () (apheleia-mode -1))))

(provide 'init-slang)
;;; init-slang.el ends here
