;;; init-theme.el ---
;;; Commentary:

;; Load theme.

;;; Code:

;; Theme load path.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Download autothemer if necessary.
(use-package autothemer)

;; Load my own theme.
(load-theme 'shapeless t)

(provide 'init-theme)
;;; init-theme.el ends here
