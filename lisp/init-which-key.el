;;; init-which-key.el ---
;;; Commentary:

;;

;;; Code:

(use-package which-key)
(require 'which-key)
(which-key-mode t)
(diminish 'which-key-mode)
(setq which-key-idle-delay 0.3)

(provide 'init-which-key)
;;; init-which-key.el ends here
