;;; init-pass.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(require 'auth-source-pass)

(leaf pass
  :require t
  :config
  (setq auth-sources '(password-store)))

(provide 'init-pass)
;;; init-pass.el ends here
