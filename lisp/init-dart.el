;;; init-dart.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf dart-mode
  :require t
  :hook (dart-mode-hook . flutter-test-mode))

;; It gives you the closing label of widgets, although it looks nice,
;; it is kind of useless.

;; (leaf lsp-dart
;;   :require t
;;   :hook
;;   ((dart-mode-hook) . lsp)
;;   )


(provide 'init-dart)
;;; init-dart.el ends here
