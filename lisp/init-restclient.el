;;; init-restclient.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; A tool for testing HTTP REST services.

;;; Code:

(elpaca
 restclient
 (require 'restclient)
 (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(provide 'init-restclient)
;;; init-restclient.el ends here
