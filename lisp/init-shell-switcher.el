;;; init-shell-switcher.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf shell-switcher
  :require t)

(setq shell-switcher-mode t)

(setq shell-switcher-new-shell-function #'shell-switcher-make-shell)

(provide 'init-shell-switcher)
;;; init-shell-switcher.el ends here
