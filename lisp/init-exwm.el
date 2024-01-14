;;; init-exwm.el --- EXWM config. -*- lexical-binding: t -*-

;;; Commentary:

;; Using Emacs as a window manager, only works on Linux.

;;; Code:

;; For bug#58245.
(setq x-no-window-manager t)

(straight-use-package 'exwm)
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

        ;; Toggle input method.
        (,(kbd "s-<SPC>") . toggle-input-method)

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

;; Do not invert the scolling direction in xmodmap, because it does
;; not work in gtk3.
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

;; (straight-use-package 'exwm-edit)
;; (require 'exwm-edit)

;; Autohide minibuffer & echo area. Don't use, it sucks.
;; (setq exwm-workspace-minibuffer-position 'bottom)

;; Set specific applications in char-mode.
(setq exwm-manage-configurations
      '(((member exwm-class-name '("slcard" "XTerm"))
         char-mode t)))

;; Firefox
;; (straight-use-package '(exwm-firefox :type git :host nil :repo "https://codeberg.org/emacs-weirdware/exwm-firefox.git"))
(straight-use-package 'exwm-firefox-core)
(straight-use-package 's)
(require 'exwm-firefox)
(keymap-set exwm-firefox-keymap "C-n" #'exwm-firefox-core-down)
(keymap-set exwm-firefox-keymap "C-p" #'exwm-firefox-core-up)
(keymap-set exwm-firefox-keymap "C-f" #'exwm-firefox-core-right)
(keymap-set exwm-firefox-keymap "C-b" #'exwm-firefox-core-left)
(keymap-set exwm-firefox-keymap "M-w" #'exwm-firefox-core-copy)
(keymap-set exwm-firefox-keymap "C-w" #'exwm-firefox-core-cut)
(keymap-set exwm-firefox-keymap "C-y" #'exwm-firefox-core-paste)
(keymap-set exwm-firefox-keymap "C-v" #'exwm-firefox-core-half-page-down)
(keymap-set exwm-firefox-keymap "M-v" #'exwm-firefox-core-half-page-up)
(keymap-set exwm-firefox-keymap "C-s" #'exwm-firefox-core-find)
(keymap-set exwm-firefox-keymap "C-g" #'exwm-firefox-core-cancel)
(keymap-set exwm-firefox-keymap "M-b" #'exwm-firefox-core-back-word)
(keymap-set exwm-firefox-keymap "M-f" #'exwm-firefox-core-forward-word)
(keymap-set exwm-firefox-keymap "M-<" #'exwm-firefox-core-top)
(keymap-set exwm-firefox-keymap "M->" #'exwm-firefox-core-bottom)
(keymap-set exwm-firefox-keymap "C-d" #'exwm-firefox-core-delete)
(keymap-set exwm-firefox-keymap "C-/" #'exwm-firefox-core-undo)
(keymap-set exwm-firefox-keymap "C-c C-k" #'exwm-firefox-core-tab-close)
(keymap-set exwm-firefox-keymap "C-a" [home])
(keymap-set exwm-firefox-keymap "C-e" [end])
(keymap-set exwm-firefox-keymap "C-k" [S-end delete])

(exwm-firefox-mode)

;; I don't know, but my firefox buffer is "firefox".
(add-to-list 'exwm-firefox-class->name-alist (cons "firefox" "firefox"))
(add-to-list 'exwm-firefox-class->name-alist (cons "firefox-esr" "firefox"))
;; My librewolf buffer is librewolf.
(add-to-list 'exwm-firefox-class->name-alist (cons "librewolf" "librewolf"))

(provide 'init-exwm)
;;; init-exwm.el ends here
