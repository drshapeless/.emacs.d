;;; init-pdf.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf pdf-tools
  :require t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq pdf-tools-handle-upgrades nil
        pdf-info-epdfinto-program "epdfinfo")
  )

(leaf pdf-view-restore
  :after pdf-tools
  :hook pdf-view-mode-hook
  :config
  (setq pdf-view-restore-filename "~/.emacs-backups/.pdf-view-restore")
  )

;; (pdf-tools-install)

(provide 'init-pdf)
;;; init-pdf.el ends here
