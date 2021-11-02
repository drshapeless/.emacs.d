;;; init-w3m.el ---
;;; Commentary:

;;

;;; Code:

(use-package w3m)
(require 'w3m-load)
;; Better not to display image as default.
;; (setq w3m-default-display-inline-images 1)

;; The ui of searx is so bad.
;; (setq w3m-search-default-engine "searx")
(setq w3m-search-default-engine "duckduckgo")

;; A bunch of key map that are similar to eww. Fuck vim keybindings.
(eval-after-load 'w3m
  '(progn
     ;; (add-to-list 'w3m-search-engine-alist
     ;;                   '("searx" "https://searx.drshapeless.com/search?q=%s"))
     (define-key w3m-mode-map "l" 'w3m-view-previous-page)
     (define-key w3m-mode-map "r" 'w3m-view-next-page)
     (define-key w3m-mode-map "n" 'w3m-next-buffer)
     (define-key w3m-mode-map "p" 'w3m-previous-buffer)
     (define-key w3m-mode-map "w" 'w3m-delete-buffer)
     ;; (define-key w3m-mode-map "g" 'w3m-reload-this-page)
     (define-key w3m-mode-map "<" 'w3m-tab-move-left)
     (define-key w3m-mode-map ">" 'w3m-tab-move-right)
     (define-key w3m-mode-map (kbd "C-c r") 'w3m-reload-this-page)
     ))

;; Disable w3m from using cookies
(setq w3m-use-cookies nil)

(provide 'init-w3m)
;;; init-w3m.el ends here
