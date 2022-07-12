;;; init-wat.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Webassembly. This package is derived from lisp mode. It will
;; magically call sly...

;; This package is not so useful outside of learning purpose. I am not
;; writing wasm by hand.

;;; Code:

(straight-use-package '(wat-mode :type git :host github :repo "devonsparks/wat-mode"))
(require 'wat-mode)

(provide 'init-wat)
;;; init-wat.el ends here
