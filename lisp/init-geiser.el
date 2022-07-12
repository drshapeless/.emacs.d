;;; init-geiser.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Scheme with guile.

;;; Code:

(straight-use-package 'geiser)
(require 'geiser)

(straight-use-package 'geiser-guile)
(require 'geiser-guile)

(provide 'init-geiser)
;;; init-geiser.el ends here
