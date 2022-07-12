;;; init-multiple-cursors.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Multiple-cursors. Very useful.

;;; Code:

(straight-use-package 'multiple-cursors)
(require 'multiple-cursors)
(keymap-global-set "C-S-c C-S-c" 'mc/edit-lines)

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
