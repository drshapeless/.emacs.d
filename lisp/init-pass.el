;;; init-pass.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Managing password in Emacs. Depends on external package `pass'.

;;; Code:

(require 'auth-source-pass)

(elpaca (password-store-otp :host github :repo "realcomplex/password-store-otp.el")
  (require 'password-store-otp))

(elpaca
    pass
  (require 'pass)
  (setq auth-sources '(password-store))
  (setq pass-username-fallback-on-filename t)
  (keymap-global-set "C-c g" #'pass))

(provide 'init-pass)
;;; init-pass.el ends here
