;;; init-dart.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Dart officially supports Emacs with lsp-mode, but lsp-mode is
;; bad. Go back to use eglot.

;;; Code:

(straight-use-package 'dart-mode)
(require 'dart-mode)
(add-hook 'dart-mode-hook #'flutter-test-mode)

(provide 'init-dart)
;;; init-dart.el ends here
