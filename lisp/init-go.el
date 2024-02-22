;;; init-go.el --- Major mode for golang -*- lexical-binding: t -*-

;;; Commentary:

;; The code is mainly from https://github.com/golang/tools/blob/master/gopls/doc/emacs.md.

;; Don't ever use space over tab in golang, golang wants tab.

;;; Code:

(elpaca
    go-mode
  (require 'go-mode)
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'go-ts-mode-hook (lambda () (setq tab-width 4)))
  (setq go-ts-mode-indent-offset 4))

(elpaca
    templ-ts-mode
  (add-hook 'templ-ts-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'templ-ts-mode-hook #'rustywind-format-on-save))

(defun rustywind-format ()
  (interactive)
  (if (f-executable-p (executable-find "rustywind"))
      (let ((tmpfile (make-nearby-temp-file "rustywind" nil nil))
            (coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8))

        (unwind-protect
            (save-restriction
              (widen)
              (write-region nil nil tmpfile)

              (let ((rustywind-args (list "--write" (file-local-name tmpfile))))
                (when (zerop (apply #'process-file (executable-find "rustywind") nil nil nil rustywind-args))
                  (insert-file-contents tmpfile nil nil nil t))))

          (delete-file tmpfile)))
    (error (format "Can't find rustywind"))))

(defun rustywind-format-on-save ()
  (add-hook 'before-save-hook #'rustywind-format nil t))

(defun drsl/go-db ()
  "Insert the snake_case version of current field "
  (interactive)
  (let ((word (string-inflection-underscore-function (current-word))))
    (end-of-line)
    (insert " `db:\"" word "\"`")))

(provide 'init-go)
;;; init-go.el ends here
