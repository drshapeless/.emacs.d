;;; init-smart-dash.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(elpaca smart-dash
  (require 'smart-dash)
  (add-hook 'c-ts-mode-hook #'smart-dash-mode)
  (add-hook 'rustic-mode-hook #'smart-dash-mode))

(provide 'init-smart-dash)
;;; init-smart-dash.el ends here
