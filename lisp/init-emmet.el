;;; init-emmet.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(elpaca emmet-mode
  (require 'emmet-mode)
  (add-hook 'templ-ts-mode-hook 'emmet-mode)
  (add-hook 'web-mode 'emmet-mode))

(provide 'init-emmet)
;;; init-emmet.el ends here
