;;; init-move-text.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Move the current region up or down

;;; Code:

(elpaca move-text
  (require 'move-text)
  (move-text-default-bindings)

  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice)
  )

(provide 'init-move-text)
;;; init-move-text.el ends here
