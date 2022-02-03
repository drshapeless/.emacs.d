;;; init-keybinding.el --- Keybindings. -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(require 'init-helpers)

(if *is-a-mac*
    (progn
      (setq mac-option-modifier 'meta)
      (setq mac-command-modifier 'super)
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

(global-set-key (kbd "s-<SPC>") 'drsl/toggle-input-and-shapeless-chinese)

;; Redefine the arrow keys to windmove.
(require 'windmove)
(global-set-key (kbd "H-s") 'windmove-left)
(global-set-key (kbd "H-f") 'windmove-right)
(global-set-key (kbd "H-e") 'windmove-up)
(global-set-key (kbd "H-d") 'windmove-down)
(global-set-key (kbd "H-a") 'other-frame)


;; (global-set-key (kbd "C-c w w") 'drsl/publish-and-sync)
;; (global-set-key (kbd "C-c w a") 'org-publish-all)
;; (global-set-key (kbd "C-z e") 'ox-slimhtml-export-to-html)

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
(global-set-key (kbd "C-c n m") 'drsl/mbsync)

;; vterm
(global-set-key (kbd "C-c v") 'vterm)

;; Toggle mood-line
(global-set-key (kbd "C-z m") 'mood-line-mode)

;; Toggle company mode.
(global-set-key (kbd "C-z t c") 'company-mode)

;; Firefox
;; (global-set-key (kbd "C-c f d") 'drsl/duckduckgo-with-firefox)
;; (global-set-key (kbd "C-c f g") 'drsl/google-with-firefox)
(global-set-key (kbd "C-c f s") 'drsl/firefox-search-duckduckgo)
(global-set-key (kbd "C-c f t") 'drsl/firefox-open-url)
(global-set-key (kbd "C-c f f") 'drsl/start-firefox)
(global-set-key (kbd "C-c f p") 'drsl/start-firefox-private)

;; Mentor, rTorrent client in Emacs
(global-set-key (kbd "C-c t") 'mentor)

;; Open password manager.
(global-set-key (kbd "C-c g") 'pass)

(if *is-a-linux*
    (progn
      (global-set-key (kbd "C-c c") 'drsl/flameshot-capture-screen)

      ;; Do not use "C-c `", as mode authors may use non alphabet characters
      ;; for shortcuts.
      (global-set-key (kbd "C-z ` `") 'drsl/monitor-off)
      (global-set-key (kbd "C-z ` d") 'drsl/powersave-off)
      (global-set-key (kbd "C-z ` k") 'drsl/remap-keyboard)
      (global-set-key (kbd "C-z ` r") 'drsl/remap-keyboard)

      ;; Audio volume in Linux
      ;; I am using pipewire and pipewire-pulseaudio
      (global-set-key (kbd "<XF86AudioLowerVolume>") 'drsl/lower-audio-volume)
      (global-set-key (kbd "<XF86AudioRaiseVolume>") 'drsl/raise-audio-volume)
      (global-set-key (kbd "C-c m v") 'drsl/show-audio-volume)
      ))

(provide 'init-keybinding)
;;; init-keybinding.el ends here
