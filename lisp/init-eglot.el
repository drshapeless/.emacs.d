;;; init-eglot.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf eglot
  :require t
  :hook
  ((c-mode-hook c++-mode-hook objc-mode-hook swift-mode-hook python-mode-hook js-mode-hook go-mode-hook sql-mode-hook dart-mode-hook) . eglot-ensure)
  (eglot-ensure . flymake)
  :bind
  (:eglot-mode-map
   ("C-c e r" . eglot-reconnect)
   ("C-c e f" . eglot-code-action-quickfix)
   ("C-c e n" . eglot-rename))
  :config
  ;; The automatic header insertion by clangd is cancer.
  (add-to-list 'eglot-server-programs
               '(c-mode . ("clangd" "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               '(c++-mode . ("clangd" "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("sourcekit-lsp")))
  (add-to-list 'eglot-server-programs
               '(sql-mode . ("sqls")))
  (add-to-list 'eglot-server-programs
               '(dart-mode . ("dart" "language-server"))))

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(setq compile-command "make")

;; Find the nearest parent go.mod as the project root.
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

(add-hook 'dart-mode-hook #'eglot-format-buffer-on-save)

(provide 'init-eglot)
;;; init-eglot.el ends here
