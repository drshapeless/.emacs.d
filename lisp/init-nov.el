;;; init-nov.el --- Epub mode in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Read epub inside Emacs.

;;; Code:

(defun drsl/browse-url-with-w3m ()
  "Quick and dirty function to use w3m as default browser in nov."
  (setq-local browse-url-browser-function 'w3m-browse-url))

(elpaca
 nov
 (require 'nov)
 (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
 (add-hook 'nov-mode-hook #'drsl/browse-url-with-w3m)
 (keymap-set nov-mode-map "q" #'kill-this-buffer)

 (defun drsl/nov-scroll-up (ARG)
   "Scroll with `View-scroll-half-page-forward' or visit next
chapter if at bottom."
   (interactive "P")
   (if (>= (window-end) (point-max))
       (nov-next-document)
     (View-scroll-half-page-forward ARG)))

 (defun drsl/nov-scroll-down (ARG)
   "Scroll with `View-scroll-half-page-backward' or visit previous
chapter if at top."
   (interactive "P")
   (if (and (<= (window-start) (point-min))
            (> nov-documents-index 0))
       (progn
         (nov-previous-document)
         (goto-char (point-max)))
     (View-scroll-half-page-backward ARG)))

 (advice-add 'nov-scroll-down :override 'drsl/nov-scroll-down)
 (advice-add 'nov-scroll-up :override 'drsl/nov-scroll-up)

 (defun drsl/nov-increase-font-size ()
   (text-scale-increase 1)
   (nov-render-document))
 )

(provide 'init-nov)
;;; init-nov.el ends here
