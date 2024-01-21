;;; init-async.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Async in Emacs, not many use, most for file copying.
;;
;; Since it initialize a new Emacs instance everytime, the init is not
;; loaded.

;;; Code:

(elpaca
 async
 (require 'async)
 (dired-async-mode 1))

(provide 'init-async)
;;; init-async.el ends here
