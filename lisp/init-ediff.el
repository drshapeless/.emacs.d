;;; init-ediff.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(provide 'init-ediff)
;;; init-ediff.el ends here
