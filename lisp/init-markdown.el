;;; init-markdown.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Markdown.

;;; Code:

(elpaca
 markdown-mode
 (require 'markdown-mode)
 (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
 (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
 (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

 (setq markdown-command "multimarkdown")
 (setq markdown-hide-urls t))

(provide 'init-markdown)
;;; init-markdown.el ends here
