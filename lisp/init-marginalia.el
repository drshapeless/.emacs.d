;;; init-marginalia.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; Enable richer annotations using the Marginalia package
(leaf marginalia
  :require t
  :init
  (marginalia-mode)
  ;; :bind (:map minibuffer-local-map
  ;;             ("M-A" . marginalia-cycle))
  )

(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)


(provide 'init-marginalia)
;;; init-marginalia.el ends here
