;;; init-markdown.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-hide-urls t))

(provide 'init-markdown)
;;; init-markdown.el ends here
