;;; init-indent-guide.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Indent guide, nothing more.

;;; Code:

(elpaca
 indent-guide
 (require 'indent-guide)
 (setq indent-guide-delay 0.2)
 (add-hook 'dart-mode-hook #'indent-guide-mode))

(provide 'init-indent-guide)
;;; init-indent-guide.el ends here
