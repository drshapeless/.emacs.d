;;; init-eglot.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Eglot is the only decent lsp server solution in Emacs.

;;; Code:

(straight-use-package 'eglot)
(require 'eglot)

;; This stops eglot from logging the json events of lsp server.
(setq eglot-events-buffer-size 0)
;; Do not show multiline eldoc.
(setq eldoc-echo-area-use-multiline-p nil)
;; Auto shutdown server
(setq eglot-autoshutdown t)

(add-hook 'c-mode-hook          #'eglot-ensure)
(add-hook 'c++-mode-hook        #'eglot-ensure)
(add-hook 'objc-mode-hook       #'eglot-ensure)
(add-hook 'swift-mode-hook      #'eglot-ensure)
(add-hook 'python-mode-hook     #'eglot-ensure)
(add-hook 'js-mode-hook         #'eglot-ensure)
(add-hook 'typescript-mode-hook #'eglot-ensure)
(add-hook 'go-mode-hook         #'eglot-ensure)
(add-hook 'sql-mode-hook        #'eglot-ensure)
(add-hook 'dart-mode-hook       #'eglot-ensure)
(add-hook 'rustic-mode-hook     #'eglot-ensure)
(add-hook 'svelte-mode-hook     #'eglot-ensure)

(keymap-set eglot-mode-map "C-c e r" #'eglot-reconnect)
(keymap-set eglot-mode-map "C-c e f" #'eglot-code-action-quickfix)
(keymap-set eglot-mode-map "C-c e n" #'eglot-rename)

(add-to-list 'eglot-server-programs
             '(c-mode . ("clangd" "--header-insertion=never")))
(add-to-list 'eglot-server-programs
             '(c++-mode . ("clangd" "--header-insertion=never")))
(add-to-list 'eglot-server-programs
             '(swift-mode . ("sourcekit-lsp")))
(add-to-list 'eglot-server-programs
             '(sql-mode . ("sqls")))
(add-to-list 'eglot-server-programs
             '(dart-mode . ("dart" "language-server")))
(add-to-list 'eglot-server-programs
             '(svelte-mode . ("svelteserver" "--stdio")))

(defun drsl/use-c-gnu-style ()
  (interactive)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "gnu"))))

;; This is stolen from llvm.
(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

;; Add a cc-mode style for editing LLVM C and C++ code
(c-add-style "llvm.org"
             '("gnu"
               (fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((arglist-intro . ++)
                                   (innamespace . 0)
                                   (member-init-intro . ++)
                                   (statement-cont . llvm-lineup-statement)))))

(defun drsl/use-c-linux-cc-llvm-style ()
  (interactive)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-mode . "linux")
                          (cc-mode . "llvm"))))

;; Use linux style on C/C++ file
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

;; LSP settings.
(setq-default eglot-workspace-configuration
              '(;; gopls config.
                (:gopls .
                        ((staticcheck . t)
                         (matcher . "CaseSensitive")))
                ;; dart config.
                (:dart .
                       ((completeFunctionCalls . t)))))

;; clang-format
(if *is-a-mac*
    (load "/opt/homebrew/share/clang/clang-format.el"))

(if *is-a-linux*
    (load "/usr/share/clang/clang-format.el"))

(require 'clang-format)

(setq-default clang-format-fallback-style "llvm")

(defun clang-format-buffer-on-save ()
  (add-hook 'before-save-hook #'clang-format-buffer -10 t))

(add-hook 'c-mode-hook #'clang-format-buffer-on-save)
(add-hook 'c++-mode-hook #'clang-format-buffer-on-save)

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

;; (add-hook 'dart-mode-hook #'eglot-format-buffer-on-save)

(defun drsl/format-buffer ()
  "Format buffer according to major mode."
  (interactive)
  (cond ((eq major-mode #'c++-mode) (clang-format-buffer))
        (t (eglot-format-buffer))))

;; Load after format-all.
(keymap-set eglot-mode-map "C-c e j" #'format-all-buffer)

(keymap-set eglot-mode-map "C-c e o" #'ff-find-other-file-other-window)
(keymap-set eglot-mode-map "C-c e p" #'ff-find-other-file)

(require 'shapeless-c-arrow)
(add-hook 'c-mode-hook #'shapeless-c-arrow-mode)
(add-hook 'c++-mode-hook #'shapeless-c-arrow-mode)

(provide 'init-eglot)
;;; init-eglot.el ends here
