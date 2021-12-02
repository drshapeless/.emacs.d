;;; init-nov.el --- Epub mode in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf nov
  :after good-scroll
  :require t
  :mode "\\.epub\\'"
  ;; :hook
  ;; (nov-mode-hook . drsl/nov-increase-font-size)
  :bind
  (:nov-mode-map
   ("C-v" . good-scroll-up)
   ("M-v" . good-scroll-down)
   ;; If you bury the buffer instead of killing it, calibredb will
   ;; create another buffer by opening the same file, AGAIN!
   ("q" . kill-this-buffer)))

(defun drsl/nov-increase-font-size ()
  (text-scale-increase 1)
  (nov-render-document))

(provide 'init-nov)
;;; init-nov.el ends here
