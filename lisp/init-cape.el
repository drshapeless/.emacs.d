;;; init-cape.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Completion backend other than company mode.

;;; Code:

;; Add extensions
(straight-use-package 'cape)
(require 'cape)

(defun drsl/setup-cape ()
  (progn
    (add-to-list 'completion-at-point-functions #'cape-file)))

(add-hook 'corfu-mode-hook #'drsl/setup-cape)

(add-hook 'makefile-gmake-mode-hook (lambda () (add-to-list 'completion-at-point-functions #'cape-dabbrev)))

(provide 'init-cape)
;;; init-cape.el ends here
