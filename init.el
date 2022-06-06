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
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; The order of the following must not change.
(require 'init-straight)
(require 'init-leaf)

(require 'init-org)

;; Emacs built-in config.
;; Although Emacs ships a version of these packages, it is usually
;; better to clone the latest version via straight.
(require 'init-tramp)
(require 'init-theme)
(require 'init-dired)
(require 'init-input-method)
(require 'init-shell)
(require 'init-epa)
;; (require 'init-erc)
(require 'init-flymake)
(require 'init-ediff)
(require 'init-ibuffer)

;; Extra packages
(require 'init-blackout)
(require 'init-mood-line)
(require 'init-yasnippet)
(require 'init-vertico)
(require 'init-orderless)
(require 'init-savehist)
(require 'init-marginalia)
(require 'init-embark)
(require 'init-consult)
(require 'init-which-key)
(require 'init-helpful)
(require 'init-aggressive-indent)
(require 'init-ripgrep)
(require 'init-shrface)
(require 'init-inherit-org)
;; (require 'init-request)
(require 'init-framemove)
(require 'init-projectile)
(require 'init-flutter)
(require 'init-cape)
(require 'init-corfu)
(require 'init-shell-switcher)
(require 'init-tree-sitter)
(require 'init-rainbow)

;; Major modes.
(require 'init-markdown)
(require 'init-swift)
(require 'init-go)
(require 'init-rust)
(require 'init-nginx)
(require 'init-web)
(require 'init-calibredb)
(require 'init-nov)
(require 'init-hackernews)
(require 'init-yaml)
(require 'init-glsl)
(require 'init-hlsl)
(require 'init-dart)

(require 'init-eglot)
(require 'init-pdf)
(require 'init-emms)
(require 'init-w3m)
(require 'init-magit)
(require 'init-vterm)
(require 'init-notmuch)
(require 'init-json)
(require 'init-lilypond)
(require 'init-mentor)
(require 'init-pass)
;; (require 'init-slime)
(require 'init-sly)

(if *is-a-linux*
    (progn
      ;; This is about smooth scrolling in Emacs.
      ;; For macos, emacs-mac has a even better scrolling.
      (if (version< emacs-version "29.0.50")
          ;; For older version, just use good-scroll.
          (require 'init-good-scroll)
        ;; pixel-scroll-precision-mode is a new feature in version 29.
        (require 'init-pixel-scroll-precision))
      (require 'init-exwm)
      )
  )
(require 'init-ui)
(require 'init-helpers)
(require 'init-keybinding)
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
   '("22aa7150cc5563723c2ff2773a38b06dc7682a7c5482b9a45670dab096385750" "baf99d71d17780801f41c0be391641d3032aa8121c2c41f379d3ec643590e6f9" "71cbf5d7f6ecdb19b43466aef10622bacbf6f5fbd924de567ddd99c52ae25230" "e449dc2222ba43d28759ffbc3b770f7186f4f5c3f9732475709f02d876ccdc8e" "143f15a0a1260761363230fadb7af13f9c76ab44dc55af47c61c789b40728484" default))
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
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
