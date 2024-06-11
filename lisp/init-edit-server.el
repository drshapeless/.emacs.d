;;; init-edit-server.el --- Edit browser textarea with Emacs. -*- lexical-binding: t -*-

;;; Commentary:

;; Press the edit button in the browser, finish with C-c C-c.

;;; Code:

(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
            (edit-server-start)
          (add-hook 'after-init-hook
                    #'(lambda() (edit-server-start))))
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (width . 80)
                  (height . 25)
                  (minibuffer . t))))

(provide 'init-edit-server)
;;; init-edit-server.el ends here
