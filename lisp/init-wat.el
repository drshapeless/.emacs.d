;;; init-wat.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Webassembly. This package is derived from lisp mode. It will
;; magically call sly...

;;; Code:

(leaf wat-mode
  :straight (wat-mode :type git :host github :repo "devonsparks/wat-mode")
  :require t)

(provide 'init-wat)
;;; init-wat.el ends here
