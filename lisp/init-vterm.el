;;; init-vterm.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf vterm
  :config
  (setq vterm-buffer-name-string "vterm %s"
        vterm-always-compile-module t))

(provide 'init-vterm)
;;; init-vterm.el ends here
