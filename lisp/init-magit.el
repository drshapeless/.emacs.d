;;; init-magit.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Without magit, I am not using git.

;;; Code:

(straight-use-package 'magit)
(require 'magit)

;; I currently don't have any use with forge.
(straight-use-package 'forge)
(require 'forge)

(provide 'init-magit)
;;; init-magit.el ends here
