;;; init-notmuch.el ---
;;; Commentary:

;;

;;; Code:

;; Notmuch is installed by package manager, so no use-package is
;; needed.
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(require 'notmuch)
;; The default dir of outmail is "sent", which doesn't match my
;; postfix setting, changed it to "Sent".
(setq notmuch-fcc-dirs "Sent +sent -unread")

(provide 'init-notmuch)
;;; init-notmuch.el ends here
