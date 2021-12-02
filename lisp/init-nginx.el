;;; init-nginx.el --- Major mode for editing nginx conf -*- lexical-binding: t -*-

;;; Commentary:

;; The mode automatically activates for:

;; Files, called nginx.conf
;; Files ending in .conf under nginx directory

;;; Code:

(leaf nginx-mode
  :require t
  :config
  (setq nginx-indent-level 2))

(provide 'init-nginx)
;;; init-nginx.el ends here
