;;; init-exwm.el --- EXWM config. -*- lexical-binding: t -*-

;;; Commentary:

;; Using Emacs as a window manager, only works on Linux.

;;; Code:

(leaf exwm
  :require t)

;; The set the default workspace into 2.
(setq exwm-workspace-number 2)

;; Make class name the buffer name.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;; Global keybindings.
(setq exwm-input-global-keys
      `(
        ;; Reset to line mode.
        ([?\H-r] . exwm-reset)

        ;; Switch workspace.
        ([?\H-w] . exwm-workspace-switch)

        ;; Launch application.
        ([?\H-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))

        ;; This is rarely used.
        ;; Switch to certain workspace.
        ;; ,@(mapcar (lambda (i)
        ;;             `(,(kbd (format "s-%d" i)) .
        ;;               (lambda ()
        ;;                 (interactive)
        ;;                 (exwm-workspace-switch-create ,i))))
        ;;           (number-sequence 0 9))

        ;; Toggle fullscreen.
        ([?\H-f] . exwm-layout-toggle-fullscreen)
        ([?\H-F] . exwm-floating-toggle-floating)

        ;; Switch to char mode.
        ([?\H-c] . exwm-input-release-keyboard)

        ;; Swap two workspace.
        ([?\H-s] . exwm-workspace-swap)

        ;; These are winmoves.
        ([?\s-a] . other-frame)
        ([?\s-s] . windmove-left)
        ([?\s-d] . windmove-down)
        ([?\s-e] . windmove-up)
        ([?\s-f] . windmove-right)

        ;; Audio volume.
        ([XF86AudioLowerVolume] . drsl/lower-audio-volume)
        ([XF86AudioRaiseVolume] . drsl/raise-audio-volume)
        ))

;; Line-editing shortcuts.
(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])

        ([?\M-b] . [C-left])
        ([?\M-f] . [C-right])
        ;; Copy and paste.
        ([?\C-w] . [?\C-x])
        ([?\C-y] . [?\C-v])
        ([?\M-w] . [?\C-c])
        ;; Search
        ([?\C-s] . [?\C-f])
        ;; Undo
        ([?\C-/] . [?\C-z])
        ))

;; Set passthrough command.
(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-u
        ?\C-h
        ?\M-x
        ?\M-`
        ?\M-&
        ?\M-:
        ;; Custom prefix.
        ?\C-z
        ))

(exwm-enable)

;; Remap keyboard.

;; This should be in .xinitrc, but for whatever reason, .xinitrc
;; always fail to do xmodmap correctly. Putting it here is guarantee
;; to work.

;; I remap left super into hyper, and change the mouse into natural
;; scrolling like macOS.

;; remove mod4 = Super_L
;; remove mod4 = Hyper_L
;; keycode 133 = Hyper_L
;; add    mod3 = Hyper_L

;; pointer = 1 2 3 5 4 7 6 8 9 10 11 12
(shell-command "xmodmap ~/.Xmodmap")

;; Use Emacs input method in X window.
;; I prefer setting the env inside Emacs instead of in .xinitrc.
(setenv "GTK_IM_MODULE" "xim")
(setenv "QT_IM_MODULE" "xim")
(setenv "XMODIFIERS" "@im=exwm-xim")
(setenv "CLUTTER_IM_MODULE" "xim")
(require 'exwm-xim)
(exwm-xim-enable)
;; Remember to set it in the passthrough list or global command.
(push ?\C-\\ exwm-input-prefix-keys)

;; Set for dual monitor.
;; Use xrandr after startx to see what your hardwares are.
(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "DP-3" 1 "HDMI-0"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output HDMI-0 --left-of DP-3 --auto")
            ))
(exwm-randr-enable)

(leaf exwm-edit
  :require t)

;; Autohide minibuffer & echo area. Don't use, it sucks.
;; (setq exwm-workspace-minibuffer-position 'bottom)

;; Set specific applications in char-mode.
(setq exwm-manage-configurations
      '(((member exwm-class-name '("slcard"))
         char-mode t)))

;; Firefox
(leaf exwm-firefox
  :straight (exwm-firefox :type git :host github :repo "ieure/exwm-firefox")
  :require t
  :bind (:exwm-firefox-keymap
         ("C-n" . exwm-firefox-core-down)
         ("C-p" . exwm-firefox-core-up)
         ("C-f" . exwm-firefox-core-right)
         ("C-b" . exwm-firefox-core-left)
         ("M-w" . exwm-firefox-core-copy)
         ("C-w" . exwm-firefox-core-cut)
         ("C-y" . exwm-firefox-core-paste)
         ("C-v" . exwm-firefox-core-half-page-down)
         ("M-v" . exwm-firefox-core-half-page-up)
         ("C-s" . exwm-firefox-core-find)
         ("C-g" . exwm-firefox-core-cancel)
         ("M-b" . exwm-firefox-core-back-word)
         ("M-f" . exwm-firefox-core-forward-word)
         ("M-<" . exwm-firefox-core-top)
         ("M->" . exwm-firefox-core-bottom)
         ("C-d" . exwm-firefox-core-delete)
         ("C-/" . exwm-firefox-core-undo)
         ("C-c C-k" . exwm-firefox-core-tab-close)
         ("C-a" . [home])
         ("C-e" . [end])
         ("C-k" . [S-end delete])
         ))

(exwm-firefox-mode)

;; I don't know, but my firefox buffer is "firefox".
(add-to-list 'exwm-firefox-class->name-alist (cons "firefox" "firefox"))

(provide 'init-exwm)
;;; init-exwm.el ends here
