;;; init-shapeless-blog.el --- shapeless blog -*- lexical-binding: t -*-

;;; Commentary:

;; A minimalistic blogging tool.

;;; Code:

(require 'shapeless-blog)

(setq slblog-username 'drshapeless)
(setq slblog-password (auth-source-pass-get 'secret "slblog/drshapeless"))
(setq slblog-api-address "https://blog.drshapeless.com/api/v1/")

(define-key org-mode-map (kbd "C-c b p") 'slblog-post)
(define-key org-mode-map (kbd "C-c b n") 'slblog-post-no-change)
(define-key org-mode-map (kbd "C-c b t") 'slblog-update-token)
(define-key org-mode-map (kbd "C-c b i") 'slblog-insert-new-blog-template)

(provide 'init-shapeless-blog)
;;; init-shapeless-blog.el ends here
