;;; init-lilypond.el ---
;;; Commentary:

;; LilyPond is a music notation software.

;;; Code:

(if *is-mac*
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/lilypond"))

(require 'lilypond-mode)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.lytex\\'" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
(add-hook 'LilyPond-mode-hook 'display-line-numbers-mode)

(provide 'init-lilypond)
;;; init-lilypond.el ends here
