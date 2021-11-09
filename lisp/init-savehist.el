;;; init-savehist.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; Persist history over Emacs restarts. Vertico sorts by history position.
(leaf savehist
  :init
  (savehist-mode))

(provide 'init-savehist)
;;; init-savehist.el ends here
