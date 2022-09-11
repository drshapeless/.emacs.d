;;; init-yasnippet.el --- Snippet config. -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(straight-use-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode t)
(keymap-unset yas-minor-mode-map "<tab>" t)
(keymap-set yas-minor-mode-map "C-<tab>" 'yas-expand)

(provide 'init-yasnippet)
