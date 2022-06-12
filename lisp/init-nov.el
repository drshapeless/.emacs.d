;;; init-nov.el --- Epub mode in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defun drsl/browse-url-with-w3m ()
  "Quick and dirty function to use w3m as default browser in nov."
  (setq-local browse-url-browser-function 'w3m-browse-url))

(leaf nov
  :require t
  :mode "\\.epub\\'"
  :hook
  (nov-mode-hook . shrface-mode)
  ;; Since nov only uses `browse-url' to browse external url, we
  ;; change the `browse-url-browser-function' to `w3m-browse-url'
  ;; without disturbing other things.
  (nov-mode-hook . drsl/browse-url-with-w3m)
  :bind
  (:nov-mode-map
   ;; ("C-v" . good-scroll-up)
   ;; ("M-v" . good-scroll-down)
   ;; If you bury the buffer instead of killing it, calibredb will
   ;; create another buffer by opening the same file, AGAIN!
   ("q" . kill-this-buffer))
  :config
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions
        (append nov-shr-rendering-functions shr-external-rendering-functions))

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
  )

(defun drsl/nov-increase-font-size ()
  (text-scale-increase 1)
  (nov-render-document))

(leaf nov-xwidget
  :straight (nov-xwidget :type git :host github :repo "chenyanming/nov-xwidget")
  :require t
  :bind ((:nov-xwidget-webkit-mode-map
          ("n" . nov-xwidget-next-document)
          ("p" . nov-xwidget-previous-document)
          ("q" . kill-this-buffer))
         (:nov-mode-map
          ("v" . nov-xwidget-view)))
  :config
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
  ;; (add-hook 'nov-mode-hook 'nov-xwidget-view)
  )

;; (if (featurep 'xwidget)

;;     (progn (add-hook 'nov-mode-hook 'shrface-mode)
;;            (add-hook 'nov-mode-hook 'drsl/browse-url-with-w3m)))

(provide 'init-nov)
;;; init-nov.el ends here
