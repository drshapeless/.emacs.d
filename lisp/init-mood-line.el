;;; init-mood-line.el --- A minimal mode line. -*- lexical-binding: t -*-

;;; Commentary:

;; A minimal mode-line alternative.

;;; Code:

(elpaca
 mood-line
 (require 'mood-line)

 (mood-line-mode 1)

 ;; Toggle mood-line
 (keymap-global-set "C-z m" #'mood-line-mode))

(provide 'init-mood-line)
;;; init-mood-line.el ends here
