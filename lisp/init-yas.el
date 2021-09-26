;;; init-yas.el ---
;;; Commentary:

;;

;;; Code:

(use-package yasnippet)
(require 'yasnippet)
(eval-after-load 'yasnippet
  '(progn
     ;; (define-key yas-minor-mode-map "TAB" nil)
     (define-key yas-minor-mode-map (kbd "<tab>") nil)
     (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)))
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-global-mode t)
(diminish 'yas-minor-mode "")


(provide 'init-yas)
;;; init-yas.el ends here
