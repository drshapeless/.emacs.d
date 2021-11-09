;;; init-blackout.el --- Hide mode name in modeline. -*- lexical-binding: t -*-

;;; Commentary:

;; I used to use diminish, but blackout is better.

;; However, I sometimes use moodline, which hides minor mode name by
;; default, blackout seems a bit useless.

;;; Code:

(leaf blackout
      :straight t)

(provide 'init-blackout)
;;; init-blackout.el ends here
