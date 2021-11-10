;;; init-theme.el --- drshapeless theme -*- lexical-binding: t -*-

;;; Commentary:

;; Load my custom theme.

;;; Code:

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(defun drsl/load-shapeless-theme ()
  (interactive)
  (load-theme 'shapeless t))

(drsl/load-shapeless-theme)

(provide 'init-theme)
;;; init-theme.el ends here
