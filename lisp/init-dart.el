;;; init-dart.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf dart-mode
  :require t
  :hook (dart-mode-hook . flutter-test-mode))

;; This is pretty useless at the moment.
;; (leaf lsp-dart
;;   :require t
;;   :config
;;   ;; (add-hook 'dart-mode-hook 'lsp)
;;   )


(provide 'init-dart)
;;; init-dart.el ends here
