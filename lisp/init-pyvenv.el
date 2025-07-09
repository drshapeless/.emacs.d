;;; init-pyvenv.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Since lsp-bridge needs python to work, and it needs a lot of python
;; dependencies. However, it is not a good idea to use pip to install
;; things globally.

;;; Code:

(elpaca pyvenv
  (require 'pyvenv)
  (pyvenv-mode 1)
  (pyvenv-activate "~/.venv"))

(provide 'init-pyvenv)
;;; init-pyvenv.el ends here
