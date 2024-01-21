;;; init-tempel.el --- template completion -*- lexical-binding: t -*-

;;; Commentary:

;; This is an replacement for yasnippets.

;;; Code:

(elpaca
 tempel
 (require 'tempel)

 (setq-default tempel-path
               (expand-file-name "templates/*.eld"
                                 (file-name-directory user-emacs-directory)))

 (keymap-global-set "M-+" #'tempel-expand)
 (keymap-global-set "M-*" #'tempel-insert)
 (keymap-global-set "C-<tab>" #'tempel-expand))

(provide 'init-tempel)
;;; init-tempel.el ends here
