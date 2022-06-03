;;; init-helpers.el --- Helper functions -*- lexical-binding: t -*-

;;; Commentary:

;; These are some helper functions for myself. The common point is
;; that they do not depend on any external library, only Emacs itself.

;;; Code:

(setq gc-cons-threshold 50000000)

;; From Bahbar, https://stackoverflow.com/questions/1774832/how-to-swap-the-buffers-in-2-windows-emacs.
(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(require 'shapeless-chinese)
(defun drsl/toggle-shapeless-chinese ()
  (interactive)
  (if (equal shapeless-chinese-mode t)
      (setq shapeless-chinese-mode nil)
    (setq shapeless-chinese-mode t)))
(defun drsl/toggle-input-and-shapeless-chinese ()
  (interactive)
  (toggle-input-method)
  (drsl/toggle-shapeless-chinese))

;; Syncing my personal website.
(defun drsl/sync-drshapeless ()
  (interactive)
  (async-shell-command "rsync -urv --delete-after ~/website/drshapeless/ jacky@drshapeless.com:~/public/drshapeless" "*rsync*")
  )
(defun drsl/publish-and-sync ()
  (interactive)
  (progn
    (org-publish-all)
    (drsl/sync-drshapeless))
  )

(defun drsl/mbsync ()
  "Sync all the mailbox."
  (interactive)
  (async-shell-command "mbsync -a" "*mbsync*"))

;; Some firefox shortcuts, make sure you are using exwm.
;; Don't use librewolf, EXWM xim doesn't work.

(defun drsl/firefox-search-duckduckgo (term)
  "Firefox duckduckgo search in a new tab."
  (interactive (list (read-string "duckduckgo: ")))
  (start-process-shell-command "firefox" nil (format "firefox --new-tab 'https://duckduckgo.com/?q=%s' " term)))

;; (defun drsl/google-with-firefox (term)
;;   "Firefox google search in a new tab."
;;   (interactive (list (read-string "google: ")))
;;   (start-process-shell-command "firefox" nil (format "firefox --new-tab 'https://google.com/search?q=%s'" term)))

(defun drsl/firefox-search-searx (term)
  "Firefox search in a new tab."
  (interactive (list (read-string "search: ")))
  (start-process-shell-command "firefox" nil
                               (format "firefox --new-tab 'https://searx.drshapeless.com/search?q=%s'" term)))

(defun drsl/firefox-open-url (url)
  "Firefox url in a new tab."
  (interactive (list (read-string "url: ")))
  (start-process-shell-command "firefox" nil
                               (format "firefox --new-tab '%s'" url)))

(defun drsl/start-firefox ()
  "Start a new firefox session."
  (interactive)
  (start-process-shell-command "firefox" nil "firefox"))

(defun drsl/start-firefox-private ()
  "Start a new firefox private window."
  (interactive)
  (start-process-shell-command "firefox" nil "firefox --private-window"))

(if *is-a-linux*
    (progn
      ;; Turn off monitor
      (defun drsl/monitor-off ()
        "Turn off the monitor."
        (interactive)
        (shell-command "sleep 1; xset dpms force off"))

      ;; Turn off power save mode.
      (defun drsl/powersave-off ()
        "Disable powersave.\nYou may want to do this after waking up your monitor."
        (interactive)
        (shell-command "xset -dpms"))

      ;; Remap the keyboard with ~/.Xmodmap
      (defun drsl/remap-keyboard ()
        "Remap the keyboard."
        (interactive)
        (shell-command "xmodmap ~/.Xmodmap"))

      (defun drsl/reset-screen-color ()
        "Reset screen color to 3600K."
        (interactive)
        (shell-command "redshift -x ; redshift -O 3600K"))

      ;; Use flameshot to capture screen
      (defun drsl/flameshot-capture-screen ()
        "Use flameshot to capture screen."
        (interactive)
        (async-shell-command "flameshot gui"))

      (defun drsl/lower-audio-volume ()
        (interactive)
        (shell-command "pactl set-sink-volume @DEFAULT_SINK@ -3%")
        (drsl/show-audio-volume)
        )
      (defun drsl/raise-audio-volume ()
        (interactive)
        (shell-command "pactl set-sink-volume @DEFAULT_SINK@ +3%")
        (drsl/show-audio-volume)
        )
      (defun drsl/show-audio-volume ()
        (interactive)
        (shell-command "pactl get-sink-volume @DEFAULT_SINK@ | grep Volume"))
      (defun drsl/start-discord ()
        (interactive)
        (async-shell-command "discord" "*discord*"))
      ))

(defun drsl/enable-ssh-over-socks ()
  "Enable ssh over socks."
  (interactive)
  (let* ((ssh-dir (concat (getenv "HOME") "/.ssh/"))
         (config (concat ssh-dir "config"))
         (config-socks (concat ssh-dir "config.socks")))
    (if (file-exists-p config-socks)
        (progn
          (copy-file config-socks config t)
          (message "ssh over socks enabled."))
      (message "no ssh socks config."))))

;; In config.socks,
;; Host drshapeless.com
;; HostName drshapeless.com
;; User git
;; ProxyCommand nc -v -x 127.0.0.1:10800 %h %p

;; Host github.com
;; HostName github.com
;; User git
;; ProxyCommand nc -v -x 127.0.0.1:10800 %h %p

;; Host home.drshapeless.com
;; HostName home.drshapeless.com
;; User jacky
;; ProxyCommand nc -v -x 127.0.0.1:10800 %h %p

;; Host *
;; ControlMaster yes

(defun drsl/disable-ssh-over-socks ()
  "Disable ssh over socks."
  (interactive)
  (let* ((ssh-dir (concat (getenv "HOME") "/.ssh/"))
         (config (concat ssh-dir "config"))
         (config-backup (concat ssh-dir "config.bak")))
    (if (file-exists-p config-backup)
        (progn
          (copy-file config-backup config t)
          (message "ssh over socks disabled."))
      (message "no ssh backup config."))))

;; In config.bak,
;; Host *
;; ControlMaster yes

(defun drsl/start-v2ray ()
  "Start v2ray daemon."
  (interactive)
  (async-shell-command "v2ray -c ~/code/v2ray-client.json" "*v2ray*"))

;; https://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
;; From user pesche.
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(defvar usd2hkd 7.83)

;; The fee for trading stocks on futubull.
(defun futubull-fee(cost number)
  (interactive "nCost: \nnNumber: ")
  (let* ((fee (+ (max (* 0.0049 number) 0.99)
                 (max (* 0.005 number) 1)
                 (* 0.003 number)))
         (total (* cost number))
         (min-price (+ (/ (* fee 2) number)
                       cost))
         (net (+ (* fee 2) total)))
    (message (format "Minimum price: %0.3f, fee: %0.3f, total: %0.3f, net: %0.3f, hkd: %0.3f"
                     min-price
                     fee
                     total
                     net
                     (* net usd2hkd)))))

(defun futubull-profit(cost price number)
  (interactive "nCost: \nnCurrent: \nnNumber: ")
  (let* ((fee (+ (max (* 0.0049 number) 0.99)
                 (max (* 0.005 number) 1)
                 (* 0.003 number)))
         (net (- (* price number)
                 (* cost number)
                 (* fee 2))))
    (message (format "Net: %0.3f, hkd: %0.3f, cost: %0.3f, price: %0.3f, fee: %0.3f"
                     net
                     (* net usd2hkd)
                     (* cost number)
                     (* price number)
                     (* fee 2)))))

(defun drsl/switch-buffer-by-prefix (PREFIX)
  "Select a buffer prefixed by PREFIX."
  (minibuffer-with-setup-hook
      (lambda ()
        (insert (concat "^" PREFIX)))
    (consult-buffer)))

(defun drsl/switch-buffer-firefox ()
  "Select a firefox window."
  (interactive)
  (drsl/switch-buffer-by-prefix "*firefox"))

(provide 'init-helpers)
;;; init-helpers.el ends here
