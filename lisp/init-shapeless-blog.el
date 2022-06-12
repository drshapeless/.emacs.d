;;; init-shapeless-blog.el --- shapeless blog -*- lexical-binding: t -*-

;;; Commentary:

;; A minimalistic blogging tool.

;;; Code:

(leaf shapeless-blog
  :after request
  :straight (shapeless-blog :type git :host github :repo "drshapeless/emacs-shapeless-blog")
  :require t
  :bind (:org-mode-map
         ("C-c b p" . shapeless-blog-create-or-update-post)))

(setq shapeless-blog-secret (password-store-get "blog.drshapeless.com"))
(setq shapeless-blog-api-url "https://blog.drshapeless.com/api")


;; (define-key org-mode-map (kbd "C-c b p") 'shapeless-blog-create-or-update-post)
;; (define-key org-mode-map (kbd "C-c b m") 'shapeless-blog-modify-old-post)
;; (define-key org-mode-map (kbd "C-c b s") 'shapeless-blog-sync)

(provide 'init-shapeless-blog)
;;; init-shapeless-blog.el ends here
