;;; init-flutter.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf flutter
  :require t
  :after dart-mode
  :bind (:dart-mode-map
         ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/opt/flutter/"))

(provide 'init-flutter)
;;; init-flutter.el ends here
