;;; init-rust.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf rust-mode
  :require t)

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(setq rust-format-on-save t)

(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

(provide 'init-rust)
;;; init-rust.el ends here
