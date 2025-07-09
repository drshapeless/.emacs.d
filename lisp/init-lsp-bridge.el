;;; init-lsp-bridge.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(elpaca
    (lsp-bridge :host github :repo "manateelazycat/lsp-bridge" :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources") :build (:not compile))
  (require 'lsp-bridge)
  (setq lsp-bridge-user-langserver-dir
        (expand-file-name "langserver" user-emacs-directory))
  ;; (add-to-list 'lsp-bridge-single-lang-server-mode-list
  ;;              '((templ-ts-mode) . "templ"))

  (add-to-list 'lsp-bridge-default-mode-hooks 'templ-ts-mode-hook)
  (setq lsp-bridge-user-multiserver-dir
        (expand-file-name "multiserver" user-emacs-directory))
  (add-to-list 'lsp-bridge-multi-lang-server-mode-list
               '((templ-ts-mode) . "templ_tailwindcss2"))
  (add-to-list 'lsp-bridge-multi-lang-server-mode-list
               '((web-mode) . "html_tailwindcss"))

  (add-hook 'lsp-bridge-mode-hook (lambda () (corfu-mode -1)))

  (keymap-set lsp-bridge-mode-map "M-." #'lsp-bridge-find-def)

  (add-hook 'templ-ts-mode-hook #'lsp-bridge-mode)
  )

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
