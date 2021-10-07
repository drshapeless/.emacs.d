;;; init-erc.el ---
;;; Commentary:

;; Irc related stuffs, including bitlbee.

;;; Code:
(require 'erc)
(setq erc-nick "drshapeless")
(setq erc-default-server "localhost")
(setq erc-default-port "6667")
(setq erc-password nil)

(defvar bitlbee-password (string-chop-newline
                          (shell-command-to-string "pass bitlbee/drshapeless")))

(add-hook 'erc-join-hook 'bitlbee-identify)
(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
  (when (and (string= "localhost" erc-session-server)
             (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
                                   (erc-default-target)
                                   bitlbee-password))))

(provide 'init-erc)
;;; init-erc.el ends here
