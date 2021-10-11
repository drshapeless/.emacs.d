;;; init-mu4e.el ---
;;; Commentary:

;; This is not in use. I shift to notmuch for email.

;;; Code:

(if *is-mac*
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/"))

(require 'mu4e)

;; (setq mu4e-change-filenames-when-moving t) ; for some reasons mbsync
                                        ; mess up without this,
                                        ; haven't tested yet

(setq
 mue4e-headers-skip-duplicates  t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 ;;  mu4e-date-format "%y/%m/%d"
 ;;  mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t	; for some reasons mbsync mess up without this, haven't tested yet
 mu4e-attachments-dir "~/Downloads"

 mu4e-maildir       "~/Maildir"   ;; top-level Maildir
 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Junk")

;; this setting allows to re-sync and re-index mail by pressing U
(setq mu4e-get-mail-command  "mbsync -a")

(setq
 message-send-mail-function   'smtpmail-send-it
 smtpmail-default-smtp-server "mail.drshapeless.com"
 smtpmail-smtp-server         "mail.drshapeless.com"
 smtpmail-smtp-service        465	; do not use 587, even though the emailwiz told us...
 smtpmail-stream-type         'ssl)

(provide 'init-mu4e)
;;; init-mu4e.el ends here
