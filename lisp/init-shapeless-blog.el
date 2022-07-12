;;; init-shapeless-blog.el --- shapeless blog -*- lexical-binding: t -*-

;;; Commentary:

;; A minimalistic blogging tool.

;;; Code:

(straight-use-package '(shapeless-blog :type git :host github :repo "drshapeless/emacs-shapeless-blog"))
(require 'shapeless-blog)
(keymap-set org-mode-map "C-c b p" #'shapeless-blog-create-or-update-post)
(keymap-set org-mode-map "C-c b t" #'shapeless-blog-update-token)

(setq shapeless-blog-secret (password-store-get "blog.drshapeless.com"))
(setq shapeless-blog-api-url "https://blog.drshapeless.com/api")

(provide 'init-shapeless-blog)
;;; init-shapeless-blog.el ends here
