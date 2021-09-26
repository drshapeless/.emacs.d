;;; init-pdf.el --- Configurations for pdf-tools
;;; Commentary:

;; I primarily use pdf-tools in night-mode, not matter day or
;; night. With the only exception of reading piano scores.

;;; Code:

(use-package pdf-tools)
(require 'pdf-tools)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(custom-set-variables
 '(pdf-tools-handle-upgrades nil))
(setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
;; (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)

;; This is useless, I can use the same color scheme as my emacs theme.
;; (setq pdf-view-midnight-colors '("#999999" . "#000000"))

;; When things break, try reinstall pdf-tools. It always happens when
;; poppler upgraded by your package manager.

;; (pdf-tools-install)

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  (setq pdf-view-restore-filename "~/.emacs-backups/.pdf-view-restore"))

(provide 'init-pdf)
;;; init-pdf.el ends here
