;;; init-good-scroll.el --- Pixel scrolling in Emacs. -*- lexical-binding: t -*-

;;; Commentary:

;; This mode is important when you want to scroll an inline image in
;; nov-mode (epub) or just org inline image.

;;; Code:

(leaf good-scroll
  :require t
  :init
  (good-scroll-mode t)
  :config
  (setq good-scroll-step 160)
  )

(provide 'init-good-scroll)
;;; init-good-scroll.el ends here
