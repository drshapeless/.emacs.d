;;; init-nov.el --- Epub mode in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defun drsl/browse-url-with-w3m ()
  "Quick and dirty function to use w3m as default browser in nov."
  (setq-local browse-url-browser-function 'w3m-browse-url))

(leaf nov
  :require t
  :mode "\\.epub\\'"
  :hook
  (nov-mode-hook . shrface-mode)
  ;; Since nov only uses `browse-url' to browse external url, we
  ;; change the `browse-url-browser-function' to `w3m-browse-url'
  ;; without disturbing other things.
  (nov-mode-hook . drsl/browse-url-with-w3m)
  :bind
  (:nov-mode-map
   ;; ("C-v" . good-scroll-up)
   ;; ("M-v" . good-scroll-down)
   ;; If you bury the buffer instead of killing it, calibredb will
   ;; create another buffer by opening the same file, AGAIN!
   ("q" . kill-this-buffer))
  :config
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions
        (append nov-shr-rendering-functions shr-external-rendering-functions))
  )

(defun drsl/nov-increase-font-size ()
  (text-scale-increase 1)
  (nov-render-document))

(provide 'init-nov)
;;; init-nov.el ends here
