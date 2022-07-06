;;; init-vterm.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf vterm
  :require t
  :config
  (setq vterm-buffer-name-string "vterm %s"
        vterm-always-compile-module t))

(leaf multi-vterm
  :require t
  :bind
  (("C-c s" . multi-vterm)
   (:vterm-mode-map
    ("C-c C-n" . multi-vterm-next)
    ("C-c C-p" . multi-vterm-prev))))

(provide 'init-vterm)
;;; init-vterm.el ends here
