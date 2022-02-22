;;; init-shrface.el --- org like experience in eww -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf shrface
  :require t
  :config
  (shrface-basic)
  (shrface-trial)
  ;; (shrface-default-keybindings)
  (setq shrface-href-versatile t)
  :bind (:shrface-mode-map
         ;; ("TAB" . shrface-next-link)
         ("<backtab>" . shr-previous-link)
         ("C-t" . shrface-toggle-bullets)
         ("C-j" . shrface-next-headline)
         ("C-k" . shrface-previous-headline)))

(provide 'init-shrface)
;;; init-shrface.el ends here
