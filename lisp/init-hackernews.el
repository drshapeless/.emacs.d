;;; init-hackernews.el --- Hackernews client in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Read hackernews in Emacs. Not so great.

;;; Code:

(elpaca
 hackernews
 (require 'hackernews)
 (setq hackernews-internal-browser-function #'w3m-browse-url))

(provide 'init-hackernews)
;;; init-hackernews.el ends here
