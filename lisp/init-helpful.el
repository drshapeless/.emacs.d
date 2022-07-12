;;; init-helpful.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Better help page.

;;; Code:

(straight-use-package 'helpful)
(require 'helpful)
;; I haven't find a way to remap key with `keymap-global-set'.
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key [remap describe-key] #'helpful-key)
(keymap-global-set "C-c C-d" #'helpful-at-point)
(keymap-global-set "C-h F" #'helpful-function)
(keymap-global-set "C-h C" #'helpful-command)
(keymap-global-set "C-h C-k" #'helpful-kill-buffers)

(provide 'init-helpful)
;;; init-helpful.el ends here
