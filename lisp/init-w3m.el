;;; init-w3m.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf w3m
  :require t
  :after inherit-org
  :hook
  (w3m-fontify-before-hook . inherit-org-w3m-headline-fontify)
  (w3m-fontify-after-hook . inherit-org-mode)
  :bind
  (:w3m-mode-map
   ("l" . w3m-view-previous-page)
   ("r" . w3m-view-next-page)
   ("n" . w3m-next-buffer)
   ("p" . w3m-previous-buffer)
   ("w" . w3m-delete-buffer)
   ("<" . w3m-tab-move-left)
   (">" . s3m-tab-move-right))
  :config
  (setq w3m-use-cookies nil
        ;; There is something encoding error when using duckduckgo.
        ;; w3m-search-default-engine "duckduckgo"
        ))

(provide 'init-w3m)
;;; init-w3m.el ends here
