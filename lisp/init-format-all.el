;;; init-format-all.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Format all the code.

;; This package has a weird behaviour of resetting cursor position of
;; two windows opening the same file. When saving the file, the cursor
;; will go to either the beginning or the other window's position.

;; Avoid using it.

;;; Code:

;; Do not use `format-all' for `go-mode'.
;; Do not hook to `prog-mode'.
(straight-use-package 'format-all)
(require 'format-all)
(add-hook 'format-all-mode-hook #'format-all-ensure-formatter)
;; (add-hook 'c-mode-hook #'format-all-mode)
;; (add-hook 'c++-mode-hook #'format-all-mode)
(add-hook 'dart-mode-hook #'format-all-mode)
(add-hook 'svelte-mode-hook #'format-all-mode)

(provide 'init-format-all)
;;; init-format-all.el ends here
