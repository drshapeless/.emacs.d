;;; init-cape.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Completion backend other than company mode.

;;; Code:

;; Add extensions
(straight-use-package 'cape)
(require 'cape)
(add-to-list 'completion-at-point-functions #'cape-file)

(provide 'init-cape)
;;; init-cape.el ends here
