;;; init-notmuch.el ---
;;; Commentary:

;;

;;; Code:

;; Notmuch is installed by package manager, so no use-package is
;; needed.
(if *is-linux*
    (add-to-list 'load-path "/usr/share/emacs/site-lisp"))

(if *is-mac*
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/notmuch"))

(require 'notmuch)
;; The default dir of outmail is "sent", which doesn't match my
;; postfix setting, changed it to "Sent".
(setq notmuch-fcc-dirs "Sent +sent -unread")
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "mail.drshapeless.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-smtp-user "drsl")

(provide 'init-notmuch)
;;; init-notmuch.el ends here
