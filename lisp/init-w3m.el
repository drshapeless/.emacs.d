;;; init-w3m.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; w3m is a great browser, but currently I am facing a duckduckgo
;; encoding issue.

;;; Code:

(elpaca
 w3m
 (require 'w3m nil t)
 (keymap-global-set "C-c j" #'w3m))

(provide 'init-w3m)
;;; init-w3m.el ends here
