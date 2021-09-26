;;; init-counsel.el ---
;;; Commentary:

;;

;;; Code:

(use-package counsel)
(require 'counsel)
(counsel-mode t)
(diminish 'counsel-mode)
;; (define-key counsel-mode-map (kbd "M-x") 'counsel-M-x)
;; (define-key counsel-mode-map (kbd "C-x b") 'counsel-ibuffer)
;; (define-key counsel-mode-map (kbd "C-x C-f") 'counsel-find-file)

;; Counsel-yank-pop fucking ruins my life, a lot of time I don't need
;; to see the yank history, because I know the previous one.
(define-key counsel-mode-map [remap yank-pop] 'yank-pop)
(define-key counsel-mode-map (kbd "C-M-y") 'counsel-yank-pop)

(provide 'init-counsel)
;;; init-counsel.el ends here
