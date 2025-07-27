;;; init-nov.el --- Epub mode in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Read epub inside Emacs.

;;; Code:

(defun drsl/browse-url-with-w3m ()
  "Quick and dirty function to use w3m as default browser in nov."
  (setq-local browse-url-browser-function 'w3m-browse-url))

(elpaca
    nov
  (require 'nov)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-hook 'nov-mode-hook #'drsl/browse-url-with-w3m))

(provide 'init-nov)
;;; init-nov.el ends here
