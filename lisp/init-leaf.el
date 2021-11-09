;;; init-leaf.el --- An alternative to use-package -*- lexical-binding: t -*-

;;; Commentary:

;; use-package is kind of the default of a config template, however,
;; it comes with some flaws. Those may just be as simple as poor
;; indentation. But due to the FSF licensing, contributing to
;; use-package is kind of hard. Luckily, "conao3" wrote an alternative
;; of use-package from scratch.

;;; Code:

;; This is the line to play with straight.el.
(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(leaf-keywords-init)

(setq leaf-defaults '(:straight t))

(provide 'init-leaf)
;;; init-leaf.el ends here
