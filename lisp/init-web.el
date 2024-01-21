;;; init-web.el --- Web mode -*- lexical-binding: t -*-

;;; Commentary:

;; Web mode is a mode for editing html templates.

;;; Code:

(elpaca
 web-mode
 (require 'web-mode)
 (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

 (define-derived-mode tmpl-mode web-mode "tmpl"
   "A temporary major mode for html template.")

 (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . tmpl-mode))

 (define-derived-mode svelte-mode web-mode "svelte"
   "A temporary major mode to fix eglot connection for svelte.")

 (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))

 (define-derived-mode templ-mode web-mode "Templ Mode" "Major mode for templ file"
   (setq tab-width 4))

 ;; (font-lock-add-keywords 'templ-mode '(("templ " . 'font-lock-keyword-face)))

 (add-to-list 'auto-mode-alist '("\\.templ\\'" . templ-mode))

 ;; (defun drsl/tab-width-2 ()
 ;;   (setq tab-width 2))

 ;; (add-hook 'svelte-mode-hook #'drsl/tab-width-2)

 (setq web-mode-engines-alist
       '(("svelte" . "\\.svelte\\'")))

 (setq web-mode-enable-auto-quoting nil)

 (setq-default web-mode-code-indent-offset 2)
 (setq-default web-mode-markup-indent-offset 2)
 (setq-default web-mode-css-indent-offset 2)
 )

(provide 'init-web)
;;; init-web.el ends here
