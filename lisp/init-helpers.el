;;; init-helpers.el --- Helper functions -*- lexical-binding: t -*-

;;; Commentary:

;; These are some helper functions for myself. The common point is
;; that they do not depend on any external library, only Emacs itself.

;; No function should be called within the package.

;;; Code:

;; Always use space to align.
;; (defadvice align-regexp (around align-regexp-with-spaces activate)
;;   (let ((indent-tabs-mode nil))
;;     ad-do-it))

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

;; Syncing my personal website.
(defun drsl/sync-drshapeless ()
  (interactive)
  (async-shell-command "rsync -urv --delete-after ~/website/web/ jacky@drshapeless.com:web" "*rsync*"))

(defun drsl/sync-blogs ()
  (interactive)
  (async-shell-command "rsync -urv --delete-after ~/website/web/blog/ jacky@drshapeless.com:web/blog" "*rsync*"))

(defun drsl/sync-others ()
  (interactive)
  (async-shell-command "rsync -urv --delete-after --exclude blog/ ~/website/web/ jacky@drshapeless.com:web" "*rsync*"))

(defun drsl/sync-from-drshapeless ()
  (interactive)
  (async-shell-command "rsync -urv --delete-after jacky@drshapeless.com:web/ ~/website/web" "*rsync*"))

(if *is-a-linux*
    (progn
      (defun drsl/lower-audio-volume ()
        (interactive)
        (shell-command "wpctl set-volume @DEFAULT_AUDIO_SINK@ 3%-")
        (drsl/show-audio-volume))
      (defun drsl/raise-audio-volume ()
        (interactive)
        (shell-command "wpctl set-volume @DEFAULT_AUDIO_SINK@ 3%+")
        (drsl/show-audio-volume))
      (defun drsl/show-audio-volume ()
        (interactive)
        (shell-command "wpctl get-volume @DEFAULT_AUDIO_SINK@"))
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
  (async-shell-command "v2ray run -c ~/code/v2ray-client.json" "*v2ray*"))

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

(defun drsl/switch-buffer-by-prefix (PREFIX)
  "Select a buffer prefixed by PREFIX."
  (minibuffer-with-setup-hook
      (lambda ()
        (insert (concat "^" PREFIX)))
    (consult-buffer)))

(defun drsl/insert-time-string ()
  "Insert current time string.

This format is for updating package modified time."
  (interactive)
  (insert (format-time-string "%a %b %e %H:%M:%S %Y (%z)")))

(defun drsl/pull-emacs-config ()
  "Pull Emacs config."
  (interactive)
  (async-shell-command (format "git -C %s pull" user-emacs-directory)))

(defun drsl/pull-org-roam ()
  "Pull org-roam directory."
  (interactive)
  (async-shell-command "git -C ~/org-roam/ pull"))

(defun drsl/pull-calibre ()
  "Pull calibre library."
  (interactive)
  (async-shell-command "git -C ~/calibre/ pull"))

(defun drsl/pull-personal ()
  "Pull personal git projects."
  (interactive)
  (drsl/pull-emacs-config)
  (drsl/pull-org-roam)
  (drsl/pull-calibre))

(defun drsl/download-all-magnet-links-in-html()
  "Download all the magnet link in the local html.

Visit a local html file and run the function.

It currently do not support external link due to javascript.

Only tested with nyaa search page."
  (interactive)
  (let ((dom (libxml-parse-html-region)))
    (mapcar
     (lambda (l)
       (mentor-download-load-magnet-link-or-url nil l))
     (remq nil (mapcar
                (lambda (x)
                  (if (and x
                           (string-match-p "^magnet"
                                           x))
                      x
                    nil))
                (mapcar
                 (lambda (x)
                   (dom-attr
                    x
                    'href))
                 (dom-by-tag dom 'a)))))))

;; Need revise for pure Pipewire.
;; (defun drsl/set-default-audio-output-piano ()
;;   "Set default audio output to piano."
;;   (interactive)
;;   (shell-command "pactl set-default-sink alsa_output.usb-Roland_Roland_Digital_Piano-00.analog-stereo"))

;; (defun drsl/set-default-audio-output-analog ()
;;   "Set default audio output to analog."
;;   (interactive)
;;   (shell-command "pactl set-default-sink alsa_output.pci-0000_09_00.4.analog-stereo"))

(defun drsl/generate-rsync-makefile ()
  "Append rsync commands to makefile at current directory."
  (interactive)
  (let* ((relative-dir
          (string-remove-suffix "/"
                                (file-relative-name
                                 (dired-current-directory)
                                 (getenv "HOME")))))
    (with-temp-buffer
      (insert
       (format
        "# The section below should be deleted before the first git commit.
.PHONY: from_mac to_mac from_artix to_artix from_home to_home
from_mac:
        rsync -urv --delete-after --exclude target/ --exclude .git/ jacky@mac.local:%s/ .

to_mac:
        rsync -urv --delete-after --exclude target/ --exclude .git/ ./ jacky@mac.local:%s

from_artix:
        rsync -urv --delete-after --exclude target/ --exclude .git/ jacky@artix.local:%s/ .

to_artix:
        rsync -urv --delete-after --exclude target/ --exclude .git/ ./ jacky@artix.local:%s

from_home:
        rsync -urv --delete-after --exclude target/ --exclude .git/ jacky@home.drshapeless.com:%s/ .

to_home:
        rsync -urv --delete-after --exclude target/ --exclude .git/ ./ jacky@home.drshapeless.com:%s
"
        relative-dir
        relative-dir
        relative-dir
        relative-dir
        relative-dir
        relative-dir))
      (append-to-file (point-min) (point-max) "makefile"))))

(defun drsl/no-rgb()
  "Turn off the RGB light of the RAM."
  (interactive)
  (shell-command "sudo modprobe i2c-dev")
  (async-shell-command "openrgb -p ~/.config/OpenRGB/dark.orp"))


(defun insert-todo-comment ()
  (interactive)
  (indent-for-tab-command)
  (insert "TODO: ")
  (back-to-indentation)
  (set-mark-command nil)
  (move-end-of-line nil)
  (comment-dwim nil))

(defun todo-comment-on-next-line ()
  "Insert a TODO comment on the next line at the proper indentation"
  (interactive)
  (move-end-of-line nil)
  (newline)
  (insert-todo-comment))

(defun drsl/mount-hdd0 ()
  "Mount /dev/sdb to /mnt/hdd0"
  (interactive)
  (async-shell-command "sudo mount /dev/sdb /mnt/hdd0"))

(defun drsl/mount-hdd1 ()
  "Mount /dev/sdb to /mnt/hdd1"
  (interactive)
  (async-shell-command "sudo mount /dev/sda1 /mnt/hdd1"))

(provide 'init-helpers)
;;; init-helpers.el ends here
