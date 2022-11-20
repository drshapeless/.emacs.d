;;; init-swift.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Swift mode is really bad in Emacs.

;;; Code:

(straight-use-package 'swift-mode)
(require 'swift-mode)
(setq swift-mode:basic-offset 2)

(provide 'init-swift)
;;; init-swift.el ends here
