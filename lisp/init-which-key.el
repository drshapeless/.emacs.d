;;; init-which-key.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf which-key
  :init
  (which-key-mode t)
  :config
  (setq which-key-idle-delay 0.3))

(provide 'init-which-key)
;;; init-which-key.el ends here
