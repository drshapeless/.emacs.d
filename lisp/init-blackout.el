;;; init-blackout.el --- Hide mode name in modeline. -*- lexical-binding: t -*-

;;; Commentary:

;; I used to use diminish, but blackout is better.

;; However, I sometimes use moodline, which hides minor mode name by
;; default, blackout seems a bit useless.

;;; Code:

(straight-use-package 'blackout)
(require 'blackout)
(blackout 'yas-minor-mode)
(blackout 'which-key-mode)
(blackout 'eldoc-mode)
(blackout 'visual-line-mode)

(provide 'init-blackout)
;;; init-blackout.el ends here
