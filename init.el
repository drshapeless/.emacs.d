;;; init.el --- Load the full configuration
;;; Commentary:

;; This init file is inspired by Steve Purcell's Emacs configuration.

;;; Code:

;; Load the "lisp" folder.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(require 'init-personal-info)
(require 'init-package)
(require 'init-use-package)
(require 'init-theme)

;; Use exwm only in linux.
(if *is-linux*
    (require 'init-exwm))
(require 'init-diminish)
(require 'init-counsel)
(require 'init-ivy)
(require 'init-company)
(require 'init-flymake)
(require 'init-swift)
(require 'init-eglot)
(require 'init-which-key)
(require 'init-helpful)
(require 'init-org)
(require 'init-pdf)
(require 'full-width-punc)
(require 'init-chinese-input)
(require 'init-yas)
;; (require 'init-mu4e)                    ; I use notmuch now.
(require 'init-dired)
(require 'init-emms)
(require 'init-async)
(require 'init-w3m)
(require 'init-magit)
(require 'init-keybind)
(require 'init-shell)
;; (require 'init-mood-line)               ; I prefer the default mode line now.
(require 'init-notmuch)
(require 'init-json)
(require 'init-lilypond)
;; (require 'init-org-roam)
(require 'init-mentor)
(require 'init-pass)
(require 'init-epa)
(require 'init-ui)
(require 'init-tramp)
(require 'init-erc)
(require 'init-projectile)
(require 'init-auto-package-update)

;; Start the daemon.
(server-start)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ecc2c2d410c0f898110e445f2afb058c0825eb48feb962feb9c8988d5f51a242" "7a43844f34da039fa9e32673de19a6aa8fb3280d17ea85946c8504483b682caa" "6693b4dc2fb9f780a6ebdf4880dc1f9263bc5f279845cc02aab464f07423af1e" "207aaff451989cf3d0e77570fa0ce72673913a78669b90ea2ca412d5ed98147e" "02e3514adc42b607e5071631d40c1c4d85acab2e5d31a41fda0b7e59d872f3e2" "441d82caac585a2f469e5285a53aef05a3cde6c8e47087792dafeca6a9ffe93c" "f77fb0c5de10357e8a8cbca6794f07258a7133c1b12c92309c883a1755a6cc77" "58f4e43b95ed3490e575477ae444c56755bb3068ca3c851637cdfe17263c1355" "a2d6d3b7996a64f9a7446d535501f281d53e702872aa87e54cac17ffcb05a5a4" "3d0d65f8278d6c62f4809a766c4776674daa2f607c87ec85656cee440b22ab6e" "59cfdbe6e605d69341aff104eaaf260ae69aea1dd776420edc41718ca2129cda" "5d14f979e9a157cf405088ccd6181a35046ff200e58957b6f5faba3d216698a7" "da2edb35f23c8685f41ecbda6c2eabb677f9a865c11596983123c96d00106eae" default))
 '(package-selected-packages
   '(auto-package-update projectile pinentry pass mentor org-roam json-mode mood-line emms ox-slimhtml yasnippet which-key w3m valign use-package swift-mode pdf-view-restore org-pdftools magit ivy-rich helpful exwm eglot diminish counsel company autothemer async))
 '(pdf-tools-handle-upgrades nil)
 '(safe-local-variable-values
   '((eval let*
           ((x
             (dir-locals-find-file default-directory))
            (this-directory
             (if
                 (listp x)
                 (car x)
               (file-name-directory x))))
           (unless
               (or
                (featurep 'swift-project-settings)
                (and
                 (fboundp 'tramp-tramp-file-p)
                 (tramp-tramp-file-p this-directory)))
             (add-to-list 'load-path
                          (concat this-directory "utils")
                          :append)
             (let
                 ((swift-project-directory this-directory))
               (require 'swift-project-settings)))
           (set
            (make-local-variable 'swift-project-directory)
            this-directory))))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "mail.drshapeless.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
