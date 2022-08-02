;;; init-keybinding.el --- Keybindings. -*- lexical-binding: t -*-

;;; Commentary:

;; The left super key is changed into hyper.

;; Originally, I change the right super key into hyper. But that does
;; not work well in other window manager other than EXWM.

;;; Code:

(require 'init-helpers)

(if *is-a-mac*
    (progn
      (setq mac-option-modifier 'meta)
      (setq mac-command-modifier 'hyper)
      (setq mac-right-command-modifier 'super)))

(keymap-global-set "C-x k" #'kill-this-buffer) ; Kill buffer directly.
(keymap-global-set "C-x C-b" #'ibuffer)	 ; Use ibuffer.
(keymap-global-set "C-c K" #'compile)
(keymap-global-set "C-c k" #'recompile)
(keymap-global-set "C-c S" #'eshell)
;; I use "C-'" for switching between shells now.
;; (keymap-global-set "C-c s" #'shell)

;; C-z was used as suspension of Emacs, in gui, it is useless, and it
;; sucks dick when you accidentally press it.
(keymap-global-unset "C-z")

;; I used to use C-z as a personal prefix key, but it is so hard to
;; press that I gave up using C-c. C-c does not collide with anything.

(keymap-global-set "H-<SPC>" #'drsl/toggle-input-and-shapeless-chinese)

;; Redefine the arrow keys to windmove.
(require 'windmove)
(keymap-global-set "s-s" #'windmove-left)
(keymap-global-set "s-f" #'windmove-right)
(keymap-global-set "s-e" #'windmove-up)
(keymap-global-set "s-d" #'windmove-down)
(keymap-global-set "s-a" #'other-frame)

;; Toggle company mode.
(keymap-global-set "C-z t c" #'company-mode)

;; Firefox
;; (keymap-global-set "C-c f d" #'drsl/duckduckgo-with-firefox)
;; (keymap-global-set "C-c f g" #'drsl/google-with-firefox)
(keymap-global-set "C-c f s" #'drsl/browser-search-duckduckgo)
(keymap-global-set "C-c f t" #'drsl/browser-open-url)
(keymap-global-set "C-c f f" #'drsl/browser-new)
(keymap-global-set "C-c f p" #'drsl/browser-new-private)
(keymap-global-set "C-c f w" #'drsl/switch-buffer-firefox-or-librewolf)
(keymap-global-set "C-c f o" #'drsl/start-firefox)
(keymap-global-set "C-c f l" #'drsl/start-firefox-private)

;; Scroll half page only.
;; (global-set-key [remap scroll-down-command] 'View-scroll-half-page-backward)
;; (global-set-key [remap scroll-up-command] 'View-scroll-half-page-forward)

(if *is-a-linux*
    (progn
      (keymap-global-set "C-c c" #'drsl/flameshot-capture-screen)
      (keymap-global-set "C-c d" #'drsl/start-discord)

      ;; Do not use "C-c `", as mode authors may use non alphabet characters
      ;; for shortcuts.
      (keymap-global-set "C-z ` `" #'drsl/monitor-off)
      (keymap-global-set "C-z ` d" #'drsl/powersave-off)
      (keymap-global-set "C-z ` k" #'drsl/remap-keyboard)
      (keymap-global-set "C-z ` r" #'drsl/remap-keyboard)

      ;; Audio volume in Linux
      ;; I am using pipewire and pipewire-pulseaudio
      (keymap-global-set "<XF86AudioLowerVolume>" #'drsl/lower-audio-volume)
      (keymap-global-set "<XF86AudioRaiseVolume>" #'drsl/raise-audio-volume)
      (keymap-global-set "C-c m v" #'drsl/show-audio-volume)
      ))


(keymap-global-set "C-z l" #'duplicate-line)

(provide 'init-keybinding)
;;; init-keybinding.el ends here
