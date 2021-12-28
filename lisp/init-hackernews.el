;;; init-hackernews.el --- Hackernews client in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf hackernews
  :require t
  :config
  (setq hackernews-internal-browser-function #'w3m-browse-url))

(provide 'init-hackernews)
;;; init-hackernews.el ends here
