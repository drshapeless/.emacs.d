;;; init-good-scroll.el --- Pixel scrolling in Emacs. -*- lexical-binding: t -*-

;;; Commentary:

;; This mode is important when you want to scroll an inline image in
;; nov-mode (epub) or just org inline image.

;; After Emacs 29, this package is no longer needed.

;;; Code:

(straight-use-package 'good-scroll)
(require 'good-scroll)
(good-scroll-mode t)
(setq good-scroll-step 160)

(provide 'init-good-scroll)
;;; init-good-scroll.el ends here
