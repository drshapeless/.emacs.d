;;; init.el --- drshapeless emacs init. -*- lexical-binding: t -*-
;;; Commentary:

;; The structure of the init file is inspired by Steve Purcell's Emacs
;; configuration.

;;; Code:

;; Personal information.
(setq user-full-name "drshapeless")
(setq user-mail-address "drsl@drshapeless.com")

;; Load the "lisp" folder.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; The order of the following must not change.
(require 'init-straight)
(require 'init-leaf)

;; Emacs built-in config.
;; Although Emacs ships a version of these packages, it is usually
;; better to clone the latest version via straight.
(require 'init-tramp)
(require 'init-theme)
(require 'init-dired)
(require 'init-input-method)
(require 'init-shell)
(require 'init-epa)
(require 'init-erc)
(require 'init-flymake)

;; Extra packages
(require 'init-blackout)
(require 'init-mood-line)
(require 'init-company)
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
(require 'init-good-scroll)
(require 'init-ripgrep)

;; Major modes.
(require 'init-markdown)
(require 'init-swift)
(require 'init-go)
(require 'init-nginx)
(require 'init-web)
(require 'init-calibredb)
(require 'init-nov)

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
(require 'init-projectile)

(require 'init-org)

(if *is-a-linux*
    (require 'init-exwm))
(require 'init-ui)
(require 'init-keybinding)

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
   '("71cbf5d7f6ecdb19b43466aef10622bacbf6f5fbd924de567ddd99c52ae25230" "e449dc2222ba43d28759ffbc3b770f7186f4f5c3f9732475709f02d876ccdc8e" "143f15a0a1260761363230fadb7af13f9c76ab44dc55af47c61c789b40728484" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
