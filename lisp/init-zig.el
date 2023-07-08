;;; init-zig.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(straight-use-package 'zig-mode)
(require 'zig-mode)
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(provide 'init-zig)
;;; init-zig.el ends here
