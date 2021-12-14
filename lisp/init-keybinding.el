;;; init-keybinding.el --- Keybindings. -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(if *is-a-mac*
    (progn
      (setq mac-right-command-modifier 'hyper)))

(global-set-key (kbd "C-x k") 'kill-this-buffer) ; Kill buffer directly.
(global-set-key (kbd "C-x C-b") 'ibuffer)	 ; Use ibuffer.
(global-set-key (kbd "C-c K") 'compile)
(global-set-key (kbd "C-c k") 'recompile)
(global-set-key (kbd "C-c S") 'eshell)
(global-set-key (kbd "C-c s") 'shell)

;; C-z was used as suspension of Emacs, in gui, it is useless, and it
;; sucks dick when you accidentally press it.
(global-unset-key (kbd "C-z"))

;; I used to use C-z as a personal prefix key, but it is so hard to
;; press that I gave up using C-c. C-c does not collide with anything.

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
(global-set-key (kbd "s-<SPC>") 'drsl/toggle-input-and-shapeless-chinese)

;; Redefine the arrow keys to windmove.
(require 'windmove)
(require 'framemove)
(global-set-key (kbd "H-s") 'windmove-left)
(global-set-key (kbd "H-f") 'windmove-right)
(global-set-key (kbd "H-e") 'windmove-up)
(global-set-key (kbd "H-d") 'windmove-down)
(global-set-key (kbd "H-a") 'other-frame)
(setq framemove-hook-into-windmove t)

;; Reboot solves the following bug. But after you restart Emacs, you
;; still need this.

;; For some stupid unknown reasons, framemove think my right monitor
;; is on the left. I just swap the shit.
(defadvice windmove-do-window-select (around framemove-do-window-select-wrapper activate)
  "Let windmove do its own thing, if there is an error, try
other frame."
  (condition-case err
      ad-do-it
    (error
     (cond ((eq 'left (ad-get-arg 0)) (fm-next-frame 'right))
           ((eq 'right (ad-get-arg 0)) (fm-next-frame 'left))
           (t (error (error-message-string err)))))))

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

(global-set-key (kbd "C-c w w") 'drsl/publish-and-sync)
(global-set-key (kbd "C-c w a") 'org-publish-all)
(global-set-key (kbd "C-z e") 'ox-slimhtml-export-to-html)

;; emms
(global-set-key (kbd "C-c m m") 'emms)
(global-set-key (kbd "C-c m <SPC>") 'emms-pause)
(global-set-key (kbd "C-c m p") 'emms-previous)
(global-set-key (kbd "C-c m n") 'emms-next)
(global-set-key (kbd "C-c m s") 'emms-shuffle)
;; Play from the beginning of current song.
(global-set-key (kbd "C-c m b")
                (lambda () (interactive) (emms-stop) (emms-start)))
(global-set-key (kbd "C-c m c") 'emms-show)
(global-set-key (kbd "C-c m d") 'emms-add-directory)
(global-set-key (kbd "C-c m f") 'emms-play-dired)

;; w3m
(global-set-key (kbd "C-c j") 'w3m)

;; Open not much
(global-set-key (kbd "C-c n n") 'notmuch)
(defun drsl/mbsync ()
  "Sync all the mailbox."
  (interactive)
  (async-shell-command "mbsync -a" "*mbsync*"))
(global-set-key (kbd "C-c n m") 'drsl/mbsync)

;; vterm
(global-set-key (kbd "C-c v") 'vterm)

;; Toggle mood-line
(global-set-key (kbd "C-z m") 'mood-line-mode)

;; Toggle company mode.
(global-set-key (kbd "C-z t c") 'company-mode)

;; Some firefox shortcuts, make sure you are using exwm.
;; Don't use librewolf, EXWM xim doesn't work.

;; (defun drsl/duckduckgo-with-firefox (term)
;;   "Firefox duckduckgo search in a new tab."
;;   (interactive (list (read-string "duckduckgo: ")))
;;   (start-process-shell-command "firefox" nil (format "firefox --new-tab 'https://duckduckgo.com/?q=%s' " term)))

;; (defun drsl/google-with-firefox (term)
;;   "Firefox google search in a new tab."
;;   (interactive (list (read-string "google: ")))
;;   (start-process-shell-command "firefox" nil (format "firefox --new-tab 'https://google.com/search?q=%s'" term)))

(defun drsl/search-with-firefox (term)
  "Firefox search in a new tab."
  (interactive (list (read-string "search: ")))
  (start-process-shell-command "firefox" nil (format "firefox --new-tab --search '%s'" term)))

(defun drsl/start-firefox ()
  "Start a new firefox session."
  (interactive)
  (start-process-shell-command "firefox" nil "firefox"))

(defun drsl/start-firefox-private ()
  "Start a new firefox private window."
  (interactive)
  (start-process-shell-command "firefox" nil "firefox --private-window"))

;; (global-set-key (kbd "C-c f d") 'drsl/duckduckgo-with-firefox)
;; (global-set-key (kbd "C-c f g") 'drsl/google-with-firefox)
(global-set-key (kbd "C-c f s") 'drsl/search-with-firefox)
(global-set-key (kbd "C-c f f") 'drsl/start-firefox)
(global-set-key (kbd "C-c f p") 'drsl/start-firefox-private)

;; keybindings for mentor, rTorrent client in Emacs
(global-set-key (kbd "C-c t") 'mentor)

;; Open password manager.
(global-set-key (kbd "C-c g") 'pass)

(if *is-a-linux*
    (progn
      ;; Use flameshot to capture screen
      (defun drsl/flameshot-capture-screen ()
        "Use flameshot to capture screen."
        (interactive)
        (async-shell-command "flameshot gui"))
      (global-set-key (kbd "C-c c") 'drsl/flameshot-capture-screen)

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
        "Reset screen color to 4500K."
        (interactive)
        (shell-command "redshift -x ; redshift -O 3600K"))

      ;; Do not use "C-c `", as mode authors may use non alphabet characters
      ;; for shortcuts.
      (global-set-key (kbd "C-z ` `") 'drsl/monitor-off)
      (global-set-key (kbd "C-z ` d") 'drsl/powersave-off)
      (global-set-key (kbd "C-z ` k") 'drsl/remap-keyboard)
      (global-set-key (kbd "C-z ` r") 'drsl/remap-keyboard)

      ;; Audio volume in Linux
      ;; I am using pipewire and pipewire-pulseaudio
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
      (global-set-key (kbd "<XF86AudioLowerVolume>") 'drsl/lower-audio-volume)
      (global-set-key (kbd "<XF86AudioRaiseVolume>") 'drsl/raise-audio-volume)
      (global-set-key (kbd "C-c m v") 'drsl/show-audio-volume)
      ))

(provide 'init-keybinding)
;;; init-keybinding.el ends here
