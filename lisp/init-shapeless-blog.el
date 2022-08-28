;;; init-shapeless-blog.el --- shapeless blog -*- lexical-binding: t -*-

;;; Commentary:

;; A minimalistic blogging tool.

;;; Code:

(straight-use-package '(shapeless-blog :type git :host github :repo "drshapeless/emacs-shapeless-blog"))
(require 'shapeless-blog)
;; This function prevent Emacs from asking password at restarting
;; computer.
(defun drsl/shapeless-blog-update-token ()
  "Fetch the secret via pass before updating token."
  (interactive)
  (setq shapeless-blog-secret (password-store-get "blog.drshapeless.com"))
  (shapeless-blog-update-token))

(setq shapeless-blog-api-url "https://blog.drshapeless.com/api")

(keymap-set org-mode-map "C-c b p" #'shapeless-blog-create-or-update-post)
(keymap-set org-mode-map "C-c b t" #'drsl/shapeless-blog-update-token)

(provide 'init-shapeless-blog)
;;; init-shapeless-blog.el ends here
