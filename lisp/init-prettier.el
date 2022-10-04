;;; init-prettier.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(straight-use-package 'prettier)

(add-hook 'svelte-mode-hook #'prettier-mode)

(provide 'init-prettier)
;;; init-prettier.el ends here
