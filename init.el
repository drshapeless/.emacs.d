;;; init.el --- drshapeless emacs init. -*- lexical-binding: t -*-
;;; Commentary:

;; The structure of the init file is inspired by Steve Purcell's Emacs
;; configuration.

;;; Code:

;; Personal information.
(setq user-full-name "Jacky Li")
(setq user-mail-address "drsl@drshapeless.com")

;; Load the "lisp" folder.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

(if *is-a-linux*
    (require 'site-gentoo nil t))

(require 'init-elpaca)

(require 'init-org)
(require 'init-ui)
(require 'init-helpers)
(require 'init-keybinding)

;; Emacs built-in packages.
(require 'init-tramp)
(require 'init-dired)
(require 'init-input-method)
(require 'init-epa)
(require 'init-flymake)
(require 'init-ediff)
(require 'init-ibuffer)
(require 'init-dictionary)

;; A lot of shells
(require 'init-shell)

;; ;;; Extra packages
(require 'init-yasnippet)
(require 'init-tempel)
(require 'init-vertico)
(require 'init-orderless)
(require 'init-savehist)
(require 'init-marginalia)
(require 'init-embark)
(require 'init-consult)
(require 'init-which-key)
(require 'init-helpful)
(require 'init-ripgrep)
(require 'init-request)
(require 'init-framemove)
(require 'init-cape)
(require 'init-corfu)
(require 'init-tree-sitter)
(require 'init-rainbow)
(require 'init-multiple-cursors)
(require 'init-string-inflection)

;;; Appearance
(require 'init-mood-line)
(require 'init-shrface)

;;; Major modes.
(require 'init-calibredb)
(require 'init-nov)
(require 'init-hackernews)
(require 'init-djvu)
(require 'init-restclient)

;;; Programming languages
(require 'init-markdown)
(require 'init-swift)
(require 'init-go)
(require 'init-rust)
(require 'init-nginx)
(require 'init-web)
(require 'init-yaml)
(require 'init-glsl)
(require 'init-dart)
(require 'init-flutter)
(require 'init-json)
(require 'init-lilypond)
(require 'init-typescript)
(require 'init-caddyfile)
(require 'init-zig)

;;; Programming helper
(require 'init-indent-guide)
(require 'init-apheleia)

;;; lsp server
(require 'init-eglot)

(require 'init-pdf)
(require 'init-emms)
(require 'init-w3m)
(require 'init-magit)
(require 'init-notmuch)
(require 'init-mentor)
(require 'init-pass)
(require 'init-sly)
(require 'init-geiser)

(when *is-a-linux*
  (require 'init-shapeless-record))

(require 'init-shapeless-blog)
(require 'init-async)

;; for whatever reason, font and theme must be at the end of init
(require 'init-theme)
(require 'init-font)

(server-start)

(message "Successfully loaded all configurations.")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((eval valign-mode t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
