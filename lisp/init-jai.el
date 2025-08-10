;;; init-jai.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Jai programming language

;;; Code:

(elpaca
    (jai-mode :host github :repo "elp-revive/jai-mode")
  (require 'jai-mode))

(provide 'init-jai)
;;; init-jai.el ends here
