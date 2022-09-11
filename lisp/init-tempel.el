;;; init-tempel.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; This is an replacement for yasnippets.

;;; Code:

(straight-use-package 'tempel)
(require 'tempel)

(setq-default tempel-path
              (expand-file-name "templates/*.eld"
                                (file-name-directory user-emacs-directory)))

(keymap-global-set "M-+" #'tempel-expand)
(keymap-global-set "M-*" #'tempel-insert)

(provide 'init-tempel)
;;; init-tempel.el ends here
