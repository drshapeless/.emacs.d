;;; init-epa.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf pinentry)

(leaf epa-file
  :straight nil
  :require t
  :after pinentry
  :config
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
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  )

;; Add these into your ./.gnupg/gpg-agent.conf
;; allow-emacs-pinentry
;; allow-loopback-pinentry

;; Setting the cache time to a ridiculously long time can save a lot
;; of your time inputting password. Make sure your computer is save.

;; max-cache-ttl 60000000
;; default-cache-ttl 60000000

(provide 'init-epa)
;;; init-epa.el ends here
