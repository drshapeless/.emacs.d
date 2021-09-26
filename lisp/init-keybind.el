;;; init-keybind.el --- Personal Global Emacs Keybindings
;;; Commentary:

;; The global keybindings for personal use.

;;; Code:

(if *is-mac*
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

(defun drsl/toggle-full-width-punc ()
  (interactive)
  (if (equal drsl/full-width-punc-mode t)
      (setq drsl/full-width-punc-mode nil)
    (setq drsl/full-width-punc-mode t)))
(defun drsl/toggle-input-and-full-width-punc ()
  (interactive)
  (toggle-input-method)
  (drsl/toggle-full-width-punc))
(global-set-key (kbd "s-<SPC>") 'drsl/toggle-input-and-full-width-punc)

;; Redefine the arrow keys to windmove.
(global-set-key (kbd "H-s") 'windmove-left)
(global-set-key (kbd "H-f") 'windmove-right)
(global-set-key (kbd "H-e") 'windmove-up)
(global-set-key (kbd "H-d") 'windmove-down)
(global-set-key (kbd "H-a") 'other-frame)

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

;; Auto-fill-mode toggle
(global-set-key (kbd "C-c p") 'auto-fill-mode)

;; Open not much
(global-set-key (kbd "C-c n n") 'notmuch)
(defun drsl/mbsync ()
  "Sync all the mailbox."
  (interactive)
  (async-shell-command "mbsync -a" "*mbsync*"))
(global-set-key (kbd "C-c n m") 'drsl/mbsync)

;; Toggle mood-line
(global-set-key (kbd "C-z m") 'mood-line-mode)

;; Toggle company mode.
(global-set-key (kbd "C-z t c") 'company-mode)

;; Some firefox shortcuts, make sure you are using exwm.
(defun drsl/duckduckgo-with-firefox (term)
  "Firefox duckduckgo search in a new tab."
  (interactive (list (read-string "duckduckgo: ")))
  (start-process-shell-command "firefox" nil (format "firefox --new-tab 'https://duckduckgo.com/?q=%s' " term)))

(defun drsl/google-with-firefox (term)
  "Firefox google search in a new tab."
  (interactive (list (read-string "google: ")))
  (start-process-shell-command "firefox" nil (format "firefox --new-tab 'https://google.com/search?q=%s'" term)))

(defun drsl/start-firefox ()
  "Start a new firefox session."
  (interactive)
  (start-process-shell-command "firefox" nil "firefox"))

(defun drsl/start-firefox-private ()
  "Start a new firefox private window."
  (interactive)
  (start-process-shell-command "firefox" nil "firefox --private-window"))

(global-set-key (kbd "C-c f d") 'drsl/duckduckgo-with-firefox)
(global-set-key (kbd "C-c f g") 'drsl/google-with-firefox)
(global-set-key (kbd "C-c f f") 'drsl/start-firefox)
(global-set-key (kbd "C-c f p") 'drsl/start-firefox-private)

;; keybindings for mentor, rTorrent client in Emacs
(global-set-key (kbd "C-c t") 'mentor)

;; Open password manager.
(global-set-key (kbd "C-c g") 'pass)

;; Minecraft console client
(defvar drsl/minecraft-console-client-directory "~/MinecraftClient/"
  "The path of the Minecraft console.
Default to \"~/MinecraftClient/\".")

(defvar drsl/minecraft-console-binary "MinecraftClient.exe"
  "The path of the minecraft console binary.")

(defun drsl/start-minecraft-console-normal ()
  "Start a minecraft console."
  (interactive)
  (async-shell-command (concat "cd " drsl/minecraft-console-client-directory " ; mono " drsl/minecraft-console-binary " Normal.ini") "*minecraft-console*"))

(defun drsl/start-minecraft-console-auto-attack ()
  "Start a minecraft console in auto attack mode."
  (interactive)
  (async-shell-command (concat "cd " drsl/minecraft-console-client-directory " ; mono " drsl/minecraft-console-binary " AutoAttack.ini") "*minecraft-console*"))

(defun drsl/start-minecraft-console-normal-bot ()
  "Start a minecraft console bot."
  (interactive)
  (async-shell-command (concat "cd " drsl/minecraft-console-client-directory " ; mono " drsl/minecraft-console-binary " Normal-bot.ini") "*minecraft-console*"))

(defun drsl/start-minecraft-console-auto-attack-bot ()
  "Start a minecraft console in auto attack mode."
  (interactive)
  (async-shell-command (concat "cd " drsl/minecraft-console-client-directory " ; mono " drsl/minecraft-console-binary " AutoAttack-bot.ini") "*minecraft-console*"))

(global-set-key (kbd "C-c i i") 'drsl/start-minecraft-console-normal)
(global-set-key (kbd "C-c i a") 'drsl/start-minecraft-console-auto-attack)
(global-set-key (kbd "C-c i b") 'drsl/start-minecraft-console-normal-bot)
(global-set-key (kbd "C-c i s") 'drsl/start-minecraft-console-auto-attack-bot)


(if *is-linux*
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

      ;; Do not use "C-c `", as mode authors may use non alphabet characters
      ;; for shortcuts.
      (global-set-key (kbd "C-z ` `") 'drsl/monitor-off)
      (global-set-key (kbd "C-z ` d") 'drsl/powersave-off)
      (global-set-key (kbd "C-z ` k") 'drsl/remap-keyboard)

      ;; Audio volume in Linux
      (defun drsl/lower-audio-volume ()
        (interactive)
        (shell-command "amixer set Master 3%- | grep 'Front Left:'")
        ;; (call-process-shell-command "amixer set Master 3%-" nil 0)
        )
      (defun drsl/raise-audio-volume ()
        (interactive)
        (shell-command "amixer set Master 3%+ | grep 'Front Left:'")
        ;; (call-process-shell-command "amixer set Master 3%+" nil 0)
        )
      (defun drsl/show-audio-volume ()
        (interactive)
        (shell-command "amixer | grep 'Front Left: Playback'"))
      (global-set-key (kbd "<XF86AudioLowerVolume>") 'drsl/lower-audio-volume)
      (global-set-key (kbd "<XF86AudioRaiseVolume>") 'drsl/raise-audio-volume)
      (global-set-key (kbd "C-c m v") 'drsl/show-audio-volume)
      ))

(provide 'init-keybind)
;;; init-keybind.el ends here
