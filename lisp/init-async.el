;;; init-async.el ---
;;; Commentary:

;;

;;; Code:

(use-package async)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(setq async-shell-command-buffer 'new-buffer)

(provide 'init-async)
;;; init-async.el ends here
