;;; init-lilypond.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Write music notation in Emacs.

;; The `LilyPond-mode' name is cancer.

;;; Code:

(require 'lilypond-mode)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.lytex\\'" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook #'turn-on-font-lock)
(add-hook 'LilyPond-mode-hook #'display-line-numbers-mode)

(provide 'init-lilypond)
;;; init-lilypond.el ends here
