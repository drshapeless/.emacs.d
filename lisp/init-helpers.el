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
  (async-shell-command "rsync -urv --delete-after ~/web/ jacky@drshapeless.com:web" "*rsync*"))
(defun drsl/sync-from-drshapeless ()
  (interactive)
  (async-shell-command "rsync -urv --delete-after jacky@drshapeless.com:web/ ~/web" "*rsync*"))

(defun drsl/publish-and-sync ()
  (interactive)
  (progn
    (org-publish-all)
    (drsl/sync-drshapeless))
  )

;;; Firefox.
(defcustom drsl/firefox-browser-command "librewolf"
  "The command for a firefox based browser.

Default is librewolf. Can change to firefox."
  :type 'string
  :options '(firefox librewolf))

(defun drsl/browser-search-duckduckgo (term)
  "Search with duckduckgo."
  (interactive (list (read-string "duckduckgo: ")))
  (cond ((string= drsl/firefox-browser-command "librewolf")
         (drsl/librewolf-search-duckduckgo term))
        ((string= drsl/firefox-browser-command "firefox")
         (drsl/firefox-search-duckduckgo term))))

(defun drsl/browser-open-url (url)
  "Open url."
  (interactive (list (read-string "url: ")))
  (cond ((string= drsl/firefox-browser-command 'librewolf) (drsl/librewolf-open-url url))
        ((string= drsl/firefox-browser-command 'firefox) (drsl/firefox-open-url url))))

(defun drsl/browser-new ()
  "Open a new browser window."
  (interactive)
  (cond ((string= drsl/firefox-browser-command 'librewolf) (drsl/start-librewolf))
        ((string= drsl/firefox-browser-command 'firefox) (drsl/start-firefox))))

(defun drsl/browser-new-private ()
  "Open a new private browser window."
  (interactive)
  (cond ((string= drsl/firefox-browser-command 'librewolf)
         (drsl/start-librewolf-private))
        ((string= drsl/firefox-browser-command 'firefox)
         (drsl/start-firefox-private))))

(defun drsl/firefox-search-duckduckgo (term)
  "Firefox duckduckgo search in a new window."
  (start-process-shell-command "firefox"
                               nil
                               (format "firefox 'https://duckduckgo.com/?q=%s' " term)))

(defun drsl/firefox-open-url (url)
  "Firefox url in a new window."
  (start-process-shell-command "firefox" nil
                               (format "firefox '%s'" url)))

(defun drsl/start-firefox ()
  "Start a new firefox session."
  (interactive)
  (start-process-shell-command "firefox" nil "firefox"))

(defun drsl/start-firefox-private ()
  "Start a new firefox private window."
  (interactive)
  (start-process-shell-command "firefox" nil "firefox --private-window"))

;;; librewolf
(defun drsl/librewolf-search-duckduckgo (term)
  "Librewolf duckduckgo search in a new window."
  ;; (interactive (list (read-string "duckduckgo: ")))
  (start-process-shell-command "librewolf" nil (format "librewolf 'https://duckduckgo.com/?q=%s' " term)))

(defun drsl/librewolf-open-url (url)
  "Librewolf url in a new window."
  (start-process-shell-command "librewolf"
                               nil
                               (format "librewolf '%s'" url)))

(defun drsl/start-librewolf ()
  "Start a new librewolf session."
  (interactive)
  (start-process-shell-command "librewolf" nil "librewolf"))

(defun drsl/start-librewolf-private ()
  "Start a new librewolf private window."
  (interactive)
  (start-process-shell-command "librewolf" nil "librewolf --private-window"))

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
        (drsl/show-audio-volume))
      (defun drsl/raise-audio-volume ()
        (interactive)
        (shell-command "pactl set-sink-volume @DEFAULT_SINK@ +3%")
        (drsl/show-audio-volume))
      (defun drsl/show-audio-volume ()
        (interactive)
        (shell-command "pactl get-sink-volume @DEFAULT_SINK@ | grep Volume | awk '{print $1, $5}'"))
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

(defun drsl/switch-buffer-firefox-or-librewolf ()
  "Select a firefox or librewolf window."
  (interactive)
  (drsl/switch-buffer-by-prefix "*\\(firefox\\)\\|\\(librewolf\\)"))

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

(defun drsl/macos-fullscreen ()
  "Make Emacs window in macos to fullscreen.

The default `toggle-frame-fullscreen' does not respect the notch
in newer Macbook, some contents are block by the front
camera. Also, it does not create a new dedicated desktop in
macos, just a weird fullscreen application blockiong other
applications in the original desktop.

This function makes use of the applescript to toggle fullscreen
in macos."
  (interactive)
  (async-shell-command "osascript -e 'tell application \"System Events\" to tell process \"Emacs\" \n set value of attribute \"AXFullScreen\" of window 1 to true \n end tell'"))

(defun drsl/macos-term-mode ()
  "A bunch of settings for better terminal emacs experience."
  (interactive)
  (corfu-terminal-mode 1)
  (menu-bar-mode -1))

(defun drsl/set-default-audio-output-piano ()
  "Set default audio output to piano."
  (interactive)
  (shell-command "pactl set-default-sink alsa_output.usb-Roland_Roland_Digital_Piano-00.analog-stereo"))

(defun drsl/set-default-audio-output-analog ()
  "Set default audio output to analog."
  (interactive)
  (shell-command "pactl set-default-sink alsa_output.pci-0000_09_00.4.analog-stereo"))

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

(provide 'init-helpers)
;;; init-helpers.el ends here
