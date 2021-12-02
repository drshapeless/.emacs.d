;;; init-eglot.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf eglot
  :require t
  :hook
  ((c-mode-hook c++-mode-hook objc-mode-hook swift-mode-hook python-mode-hook js-mode-hook go-mode-hook) . eglot-ensure)
  (eglot-ensure . flymake)
  :bind
  (:eglot-mode-map
   ("C-c r" . elgot-reconnect))
  :config
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("sourcekit-lsp")))
  (add-to-list 'eglot-server-programs
               '(sql-mode . ("sqls"))))

(setq js-indent-level 2)
(setq compile-command "make")

;; golang related config.
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

;; gopls config.
(setq-default eglot-workspace-configuration
              '((:gopls .
                        ((staticcheck . t)
                         (matcher . "CaseSensitive")))))

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

(provide 'init-eglot)
;;; init-eglot.el ends here
