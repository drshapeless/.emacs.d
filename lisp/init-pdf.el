;;; init-pdf.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; View pdf inside Emacs.

;; This may break when you update your system. Use `pdf-tools-install'
;; can fix it.

;;; Code:

(elpaca
 pdf-tools
 (require 'pdf-tools)
 (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
 (setq pdf-tools-handle-upgrades nil
       pdf-info-epdfinto-program "epdfinfo")
 (add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode)
 ;; (pdf-tools-install t nil t)
 ;; Fix blurry pdf on mac.
 (if *is-a-mac*
     (setq pdf-view-use-scaling t)))

(elpaca
 pdf-view-restore
 (require 'pdf-view-restore)
 (add-hook 'pdf-view-mode-hook #'pdf-view-restore-mode)
 (setq pdf-view-restore-filename
       (expand-file-name "backup/.pdf-view-restore" user-emacs-directory)))

(provide 'init-pdf)
;;; init-pdf.el ends here
