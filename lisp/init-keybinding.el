;;; init-keybinding.el --- Keybindings. -*- lexical-binding: t -*-

;;; Commentary:

;; The left Super key is changed into Hyper.
;;
;; On Linux, on keybinding should bind the the left Hyper because it
;; is reserved for Sway leading modifier.

;;; Code:

(require 'init-helpers)

(if *is-a-mac*
    (progn
      (setq mac-option-modifier 'meta)
      (setq mac-command-modifier 'hyper)
      (setq mac-right-command-modifier 'super)))

(keymap-global-set "C-x k" #'kill-this-buffer) ; Kill buffer directly.
(keymap-global-set "C-x C-b" #'ibuffer)        ; Use ibuffer.
(keymap-global-set "C-c K" #'compile)
(keymap-global-set "C-c k" #'recompile)
(keymap-global-set "C-c S" #'eshell)
;; I use "C-'" for switching between shells now.

;; C-z was used as suspension of Emacs, in GUI, it is useless, and it
;; sucks when you accidentally press it.
(keymap-global-unset "C-z")

;; I used to use C-z as a personal prefix key, but it is so hard to
;; press that I gave up using C-c. C-c does not collide with anything.

;; Redefine the arrow keys to windmove.
(require 'windmove)
(keymap-global-set "s-s" #'windmove-left)
(keymap-global-set "s-f" #'windmove-right)
(keymap-global-set "s-e" #'windmove-up)
(keymap-global-set "s-d" #'windmove-down)
(keymap-global-set "s-a" #'other-frame)

;; Scroll half page only.
;; (global-set-key [remap scroll-down-command] 'View-scroll-half-page-backward)
;; (global-set-key [remap scroll-up-command] 'View-scroll-half-page-forward)

(keymap-global-set "C-z l" #'duplicate-line)
(keymap-global-set "C-z w" #'swap-buffers-in-windows)
(keymap-global-set "C-z a" #'align-regexp)

(provide 'init-keybinding)
;;; init-keybinding.el ends here
