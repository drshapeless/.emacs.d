;;; init-which-key.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Should a list of keybindings with the current inputed prefix.

;;; Code:

(elpaca
 which-key
 (require 'which-key)
 (which-key-mode t)
 (setq which-key-idle-delay 0.3))

(provide 'init-which-key)
;;; init-which-key.el ends here
