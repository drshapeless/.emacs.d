;;; init-cape.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Completion backend other than company mode.

;;; Code:

;; Add extensions
(elpaca
    cape
  (require 'cape)

  (add-hook 'makefile-gmake-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-dabbrev)))

  (add-hook 'makefile-bsdmake-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-dabbrev))))


(provide 'init-cape)
;;; init-cape.el ends here
