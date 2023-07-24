;;; init-cape.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Completion backend other than company mode.

;;; Code:

;; Add extensions
(straight-use-package 'cape)
(require 'cape)

(defun drsl/setup-cape ()
  (progn
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

(add-hook 'corfu-mode-hook #'drsl/setup-cape)

(provide 'init-cape)
;;; init-cape.el ends here
