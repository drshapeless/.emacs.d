;;; init-geiser.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Scheme with guile.

;;; Code:

(elpaca
 geiser
 (require 'geiser))

(elpaca
 geiser-guile
 (require 'geiser-guile))

(provide 'init-geiser)
;;; init-geiser.el ends here
