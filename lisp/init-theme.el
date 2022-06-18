;;; init-theme.el --- drshapeless theme -*- lexical-binding: t -*-

;;; Commentary:

;; Load my custom theme.

;;; Code:

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun drsl/load-shapeless-theme ()
  (interactive)
  (load-theme 'shapeless t))

(drsl/load-shapeless-theme)

(provide 'init-theme)
;;; init-theme.el ends here
