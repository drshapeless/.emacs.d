;;; init-web.el --- Web mode -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf web-mode
  :require t
  :mode (("\\.phtml\\'" "\\.tmpl\\'") . web-mode))

(provide 'init-web)
;;; init-web.el ends here
