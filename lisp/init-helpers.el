;;; init-helpers.el --- Helper functions -*- lexical-binding: t -*-

;;; Commentary:

;; These are some helper functions for myself. The common point is
;; that they do not depend on any external library, only Emacs itself.

;;; Code:

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

      ))

(defun drsl/toggle-ssh-over-socks ()
  "Toggle ssh over socks.

The function requires a 'config.bak' and 'config.socks' in the
ssh directory. "
  (interactive)
  (let* ((ssh-dir (concat (getenv "HOME") "/.ssh/"))
         (config (concat ssh-dir "config"))
         (config-socks (concat ssh-dir "config.socks"))
         (config-backup (concat ssh-dir "config.bak")))
    (if (file-exists-p config-socks)
        (progn
          (copy-file config-socks config)
          (message "ssh over socks enabled."))
      (if (file-exists-p config-backup)
          (progn
            (copy-file config-backup config)
            (message "ssh over socks disabled."))
        (message "no backup ssh config file.")))))

(setq gc-cons-threshold 50000000)

(provide 'init-helpers)
;;; init-helpers.el ends here
