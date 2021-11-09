;;; init-yasnippet.el --- Snippet config. -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf yasnippet
  :require t
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode t)
  :bind ((:yas-minor-mode-map
          ("<tab>" . nil)
          ("C-<tab>" . yas-expand))))

(provide 'init-yasnippet)
