;;; init-ibuffer.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf ibuffer-project
  :require t)

(add-hook 'ibuffer-hook
          (lambda ()
            (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
