;;; init-exwm.el --- EXWM config. -*- lexical-binding: t -*-

;;; Commentary:

;; Using Emacs as a window manager, only works on Linux.

;;; Code:

(leaf exwm)

(require 'exwm)

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
        ([?\s-r] . exwm-reset)
        ;; Switch workspace.
        ([?\s-w] . exwm-workspace-switch)
        ;; Launch application.
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ;; Switch to certain workspace.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Toggle fullscreen.
        ([?\s-f] . exwm-layout-toggle-fullscreen)
        ([?\s-F] . exwm-floating-toggle-floating)
        ;; Switch to char mode.
        ([?\s-c] . exwm-input-release-keyboard)
        ;; Swap two workspace.
        ([?\s-s] . exwm-workspace-swap)
        ;; These are winmoves.
        ([?\H-a] . other-frame)
        ([?\H-s] . windmove-left)
        ([?\H-d] . windmove-down)
        ([?\H-e] . windmove-up)
        ([?\H-f] . windmove-right)
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
        ;; Input method.
        ?\C-\\
        ))

(exwm-enable)

;; Remap keyboard.

;; This should be in .xinitrc, but for whatever reason, .xinitrc
;; always fail to do xmodmap correctly. Putting it here is guarantee
;; to work.

;; I remap right super into hyper, and change the mouse into natural
;; scrolling like macOS.
(shell-command "xmodmap ~/.Xmodmap")

;; Use Emacs input method in X window. Remember to set it in the
;; passthrough list or global command.
(require 'exwm-xim)
(exwm-xim-enable)

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

;; Autohide minibuffer & echo area. Don't use, it sucks.
;; (setq exwm-workspace-minibuffer-position 'bottom)

;; Set specific applications in char-mode.
(setq exwm-manage-configurations
      '(((member exwm-class-name '("slcard"))
         char-mode t)))

(provide 'init-exwm)
;;; init-exwm.el ends here
