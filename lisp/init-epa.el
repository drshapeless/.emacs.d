;;; init-epa.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf epa-file
  :straight nil
  :require t
  :config
  (setq epa-file-select-keys nil
        epa-file-encrypt-to '("drsl@drshapeless.com"))
  ;; Fix EasyPG error.
  ;; From https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html.
  (setq epa-pinentry-mode 'loopback)
  )

(leaf pinentry
  :init
  (pinentry-start)
  :config
  (setenv "GPG_AGENT_INFO" nil))

(provide 'init-epa)
;;; init-epa.el ends here
