;;; init-shapeless-blog.el --- shapeless blog -*- lexical-binding: t -*-

;;; Commentary:

;; A minimalistic blogging tool.

;;; Code:

(leaf shapeless-blog
  :straight (shapeless-blog :type git :host github :repo "drshapeless/emacs-shapeless-blog")
  :require t
  :config
  (setq shapeless-blog-remote-path "jacky@drshapeless.com:shapeless-blog")
  :bind (:org-mode-map
         ("C-c b p" . shapeless-blog-create-or-update-post)
         ("C-c b m" . shapeless-blog-modify-old-post)
         ("C-c b s" . shapeless-blog-sync)))

;; (define-key org-mode-map (kbd "C-c b p") 'shapeless-blog-create-or-update-post)
;; (define-key org-mode-map (kbd "C-c b m") 'shapeless-blog-modify-old-post)
;; (define-key org-mode-map (kbd "C-c b s") 'shapeless-blog-sync)

(provide 'init-shapeless-blog)
;;; init-shapeless-blog.el ends here
