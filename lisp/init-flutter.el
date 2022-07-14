;;; init-flutter.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Flutter utilities.

;; This package should be after dart.

;;; Code:

(straight-use-package 'flutter)
(require 'flutter)
(keymap-set dart-mode-map "C-M-x" #'flutter-run-or-hot-reload)
(if *is-a-linux*
    (setq flutter-sdk-path "~/src/flutter/"))
(if *is-a-mac*
    (setq flutter-sdk-path "/opt/homebrew/Caskroom/flutter/3.0.0/flutter"))

(provide 'init-flutter)
;;; init-flutter.el ends here
