;;; init-lilypond.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(if *is-a-mac*
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/lilypond"))

(leaf lilypond-mode
  :straight nil
  :require t
  :mode (("\\.ly\\'" "\\.ily\\'" "\\.lytex\\'") . LilyPond-mode)
  :hook
  (LilyPond-mode-hook . (lambda () (turn-on-font-lock)))
  (LilyPond-mode-hook . display-line-numbers-mode))

(provide 'init-lilypond)
;;; init-lilypond.el ends here
