;;; init-savehist.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Persist history over Emacs restarts.

;;; Code:

;; Vertico sorts by history position.
(require 'savehist)
(savehist-mode t)

(provide 'init-savehist)
;;; init-savehist.el ends here
