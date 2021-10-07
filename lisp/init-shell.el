;;; init-shell.el ---
;;; Commentary:

;;

;;; Code:

;; Disable shell from echoing.
(add-hook 'comint-mode-hook (lambda () (setq comint-process-echoes t)))

;; Use zsh as default shell in Emacs.
(setq explicit-shell-file-name "/bin/zsh")
(setenv "SHELL" "/bin/zsh")

(provide 'init-shell)
;;; init-shell.el ends here
