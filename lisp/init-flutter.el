;;; init-flutter.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Flutter utilities.

;; This package should be after dart.

;;; Code:

(straight-use-package 'flutter)
(require 'flutter)
(keymap-set dart-mode-map "C-M-x" #'flutter-run-or-hot-reload)
(setq flutter-sdk-path "~/src/flutter/")

(provide 'init-flutter)
;;; init-flutter.el ends here
