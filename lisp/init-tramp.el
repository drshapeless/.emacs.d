;;; init-tramp.el ---
;;; Commentary:

;;

;;; Code:

;; This is for remote host to recognise tramp as a unique type of
;; shell, useful for remote zsh, e.g. macOS.
(setq tramp-terminal-type "tramp")

;; From 2021-10-21, the following method does not work.
;; In order to use gnuls on a remote mac, just symlink gls to ls in
;; /usr/local/bin.

;; (require 'tramp-sh)
;; ;; Use gls if found in system, useful when in macOS.
;; (defun tramp-get-ls-command (vec)
;;   "Determine remote `ls' command."
;;   (with-tramp-connection-property vec "ls"
;;     (tramp-message vec 5 "Finding a suitable `ls' command")
;;     (or
;;      (catch 'ls-found
;;        (dolist (cmd '("gls" "ls" "gnuls"))
;;          (let ((dl (tramp-get-remote-path vec))
;;                result)
;;            (while (and dl (setq result (tramp-find-executable vec cmd dl t t)))
;;              ;; Check parameters.  On busybox, "ls" output coloring is
;;              ;; enabled by default sometimes.  So we try to disable it
;;              ;; when possible.  $LS_COLORING is not supported there.
;;              ;; Some "ls" versions are sensitive to the order of
;;              ;; arguments, they fail when "-al" is after the
;;              ;; "--color=never" argument (for example on FreeBSD).
;;              (when (tramp-send-command-and-check
;;                     vec (format "%s -lnd /" result))
;;                (when (tramp-send-command-and-check
;;                       vec (format
;;                            "%s --color=never -al %s"
;;                            result (tramp-get-remote-null-device vec)))
;;                  (setq result (concat result " --color=never")))
;;                (throw 'ls-found result))
;;              (setq dl (cdr dl))))))
;;      (tramp-error vec 'file-error "Couldn't find a proper `ls' command")))))

(provide 'init-tramp)
;;; init-tramp.el ends here
