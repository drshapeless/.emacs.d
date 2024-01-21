;;; init-shrface.el --- org like experience in eww -*- lexical-binding: t -*-

;;; Commentary:

;; Make eww display like org mode.

;;; Code:

(elpaca
 shrface
 (require 'shrface)
 (shrface-basic)
 (shrface-trial)
 (setq shrface-href-versatile t)
 (keymap-set shrface-mode-map "<backtab>" #'shr-previous-link)
 (keymap-set shrface-mode-map "C-t" #'shrface-toggle-bullets)
 (keymap-set shrface-mode-map "C-j" #'shrface-next-headline)
 (keymap-set shrface-mode-map "C-k" #'shrface-previous-headline))

(provide 'init-shrface)
;;; init-shrface.el ends here
