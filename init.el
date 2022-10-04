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
(defconst *is-older-emacs* (version< emacs-version "29.0.50"))

(require 'init-compat)
(require 'init-keybinding)

;; The order of the following must not change.
(require 'init-straight)
;; (require 'init-leaf)

(require 'init-org)

;; Emacs built-in packages.
(require 'init-tramp)
(require 'init-theme)
(require 'init-dired)
(require 'init-input-method)
(require 'init-shell)
(require 'init-epa)
;; (require 'init-erc)                     ; It is not useful in modern age.
(require 'init-flymake)
(require 'init-ediff)
(require 'init-ibuffer)
(require 'init-dictionary)

;;; Extra packages
(require 'init-blackout)                ; This is useless as I am using moodline.
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
;; (require 'init-projectile)
(require 'init-cape)
(require 'init-corfu)
(require 'init-tree-sitter)
(require 'init-rainbow)
(require 'init-multiple-cursors)
(require 'init-aggressive-indent)

;;; Appearance
(require 'init-mood-line)
(require 'init-shrface)
(require 'init-inherit-org)

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
(require 'init-wat)
;; (require 'init-svelte)
(require 'init-json)
(require 'init-lilypond)
(require 'init-typescript)
(require 'init-caddyfile)

;;; Programming helper
(require 'init-indent-guide)
(require 'init-format-all)
(require 'init-prettier)

;;; lsp server
(require 'init-eglot)
;; (require 'init-lsp)                     ; lsp is still crap
;; (require 'init-lsp-bridge)

(require 'init-pdf)
(require 'init-emms)
(require 'init-w3m)
(require 'init-magit)
(require 'init-notmuch)
(require 'init-mentor)
(require 'init-pass)
(require 'init-sly)
(require 'init-geiser)

(if *is-a-linux*
    (progn
      ;; This is about smooth scrolling in Emacs.
      ;; For macos, emacs-mac has a even better scrolling.
      (if *is-older-emacs*
          ;; For older version, just use good-scroll.
          (require 'init-good-scroll)
        ;; pixel-scroll-precision-mode is a new feature in version 29.
        (require 'init-pixel-scroll-precision))
      (require 'init-exwm)
      )
  )

(require 'init-font)
(require 'init-helpers)
(require 'init-ui)
(require 'init-shapeless-blog)

(server-start)

(message "Successfully loaded all configurations.")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b8eff8830e1002339c922bef5858f65e17555795a82dc0db4e943336f9c8f84d" default))
 '(safe-local-variable-values
   '((diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
