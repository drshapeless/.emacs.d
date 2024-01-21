;;; init-zig.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; zig programming language

;;; Code:

(elpaca
 zig-mode
 (require 'zig-mode)
 (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

(provide 'init-zig)
;;; init-zig.el ends here
