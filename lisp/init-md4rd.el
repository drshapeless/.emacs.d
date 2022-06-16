;;; init-md4rd.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Reddit client in Emacs.

;;; Code:

(leaf md4rd
  :require t
  :hook
  (md4rd-mode-hook . md4rd-indent-all-the-lines)
  :config
  (setq md4rd-subs-active '(emacs golang archlinux rust)))

(provide 'init-md4rd)
;;; init-md4rd.el ends here
