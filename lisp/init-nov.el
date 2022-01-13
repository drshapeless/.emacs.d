;;; init-nov.el --- Epub mode in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf nov
  :require t
  :mode "\\.epub\\'"
  :hook
  (nov-mode-hook . shrface-mode)
  :bind
  (:nov-mode-map
   ("C-v" . good-scroll-up)
   ("M-v" . good-scroll-down)
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
