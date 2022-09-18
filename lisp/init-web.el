;;; init-web.el --- Web mode -*- lexical-binding: t -*-

;;; Commentary:

;; Web mode is a mode for editing html templates.

;;; Code:

(straight-use-package 'web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))

(define-derived-mode svelte-mode web-mode "svelte"
  "A temporary major mode to fix eglot connection.")

(add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))
(defun drsl/tab-width-2 ()
  (setq tab-width 2))
(add-hook 'svelte-mode-hook #'drsl/tab-width-2)

(setq web-mode-engines-alist
      '(("svelte" . "\\.svelte\\'")))

(setq web-mode-enable-auto-quoting nil)

(provide 'init-web)
;;; init-web.el ends here
