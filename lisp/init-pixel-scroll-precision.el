;;; init-pixel-scroll-precision.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; From Emacs 29, it comes with a built in `pixel-scroll-precision-mode'.

;; If you are using gtk3, setting xmodmap does not reverse the
;; scrolling direction, you have to set it in X config.

;;; Code:

(pixel-scroll-precision-mode 1)

(provide 'init-pixel-scroll-precision)
;;; init-pixel-scroll-precision.el ends here
