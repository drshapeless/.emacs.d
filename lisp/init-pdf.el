;;; init-pdf.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; View pdf inside Emacs.

;; This may break when you update your system. Use `pdf-tools-install'
;; can fix it.

;;; Code:

(straight-use-package 'pdf-tools)
(require 'pdf-tools)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(setq pdf-tools-handle-upgrades nil
      pdf-info-epdfinto-program "epdfinfo")

(straight-use-package 'pdf-view-restore)
(require 'pdf-view-restore)
(add-hook 'pdf-view-mode-hook #'pdf-view-restore-mode)
(setq pdf-view-restore-filename "~/.emacs.d/backup/.pdf-view-restore")

(provide 'init-pdf)
;;; init-pdf.el ends here
