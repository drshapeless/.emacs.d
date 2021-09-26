;;; init-epa.el ---
;;; Commentary:

;; gpg in Emacs.

;;; Code:

(require 'epa-file)
(setq epa-file-select-keys nil)
(setq epa-file-encrypt-to '("drsl@drshapeless.com"))

;; Fix EasyPG error.
;; From https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html.
(setq epa-pinentry-mode 'loopback)

;; This will allow emacs to enter gpg passphase, even in Emacs shell.
(use-package pinentry)
(pinentry-start)
;; This prevent GPG from using its own external pinentry and force
;; Emacs to handle the pinentry.
(setenv "GPG_AGENT_INFO" nil)

(provide 'init-epa)
;;; init-epa.el ends here
