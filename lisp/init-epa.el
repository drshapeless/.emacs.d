;;; init-epa.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; This must be after `org'.

;; This config allows inputing password of, e.g. gpg, inside emacs
;; echo area. Otherwise, a pop up window is shown, which is also
;; usable, but not as great.

;; Add these into your ./.gnupg/gpg-agent.conf
;; allow-emacs-pinentry
;; allow-loopback-pinentry

;; Setting the cache time to a ridiculously long time can save a lot
;; of your time inputting password. Do not use do this on shared computer.

;; max-cache-ttl 60000000
;; default-cache-ttl 60000000

;;; Code:

(straight-use-package 'pinentry)
(require 'pinentry)

(require 'epa-file)
(setq epa-file-select-keys nil
      epa-file-encrypt-to '("drsl@drshapeless.com"))
;; From https://github.com/ch11ng/exwm/wiki
;; I am not putting them in the exwm file.
(setq epa-pinentry-mode 'loopback)
(setq epg-pinentry-mode 'loopback)
(setenv "GPG_AGENT_INFO" nil)
(setq auth-source-debug t)
(epa-file-enable)
(pinentry-start)
;; This has to be after org.
(require 'org-crypt)
(org-crypt-use-before-save-magic)

(provide 'init-epa)
;;; init-epa.el ends here
