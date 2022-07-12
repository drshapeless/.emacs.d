;;; init-notmuch.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; Notmuch is installed by package manager, so no use-package is
;; needed.
(if *is-a-linux*
    (add-to-list 'load-path "/usr/share/emacs/site-lisp"))

(if *is-a-mac*
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/notmuch"))

(require 'notmuch)
;; The default dir of outmail is "sent", which doesn't match my
;; postfix setting, changed it to "Sent".
(setq notmuch-fcc-dirs "Sent +sent -unread")
(setq notmuch-show-logo nil)
(setq-default notmuch-search-oldest-first nil)

(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "mail.drshapeless.com"
      smtpmail-smtp-service 587
      smtpmail-smtp-user "drsl")

;; Open not much
(keymap-global-set "C-c n n" #'notmuch)

;; Sync email
(defun drsl/mbsync ()
  "Sync all the mailbox."
  (interactive)
  (async-shell-command "mbsync -a" "*mbsync*"))

(keymap-global-set "C-c n m" #'drsl/mbsync)

(provide 'init-notmuch)
;;; init-notmuch.el ends here
