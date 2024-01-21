;;; init-multiple-cursors.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Multiple-cursors. Very useful.

;;; Code:

(elpaca
 multiple-cursors
 (require 'multiple-cursors)
 (keymap-global-set "C-S-c C-S-c" 'mc/edit-lines)
 (keymap-global-set "C->" 'mc/mark-next-like-this)
 (keymap-global-set "C-c C->" 'mc/mark-all-like-this))

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
