;;; init-ui.el --- User interface config. -*- lexical-binding: t -*-

;;; Commentary:

;; User interface settings.

;;; Code:

(when *is-a-linux*
  ;; Disable menu bar.

  ;; Do not disable menu bar in macos, it will cause weird
  ;; behaviour of the fullscreen button.
  (menu-bar-mode -1))

(setq inhibit-startup-screen t)		; Disable startup screen.
(scroll-bar-mode -1)                    ; Disable scroll bar.
(tool-bar-mode -1)                      ; Disable tool bar.

(setq display-time-format "%b %e %H:%M")

;; This line prevent the mode line from showing useless CPU usage.
(setq display-time-load-average-threshold 10.0)
(display-time-mode t)                   ; Display time.

(column-number-mode t)
(blink-cursor-mode -1)                  ; Disable cursor blinking.
(global-visual-line-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(setq ring-bell-function 'ignore)	; No notification sound.
(pixel-scroll-precision-mode 1)         ; smooth scrolling

;; Auto-revert doesn't work well with tramp.
(global-auto-revert-mode t)

;; (set-frame-parameter nil 'fullscreen 'fullboth)
(setq confirm-kill-processes nil)

;; Cleanup whitespace before saving.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Enable line number mode in programming modes.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable horizontal scroll
;; Do not use horizontal scroll, it is very annoying.
;; (setq mouse-wheel-tilt-scroll t)

;; Never use tab.
(setq-default indent-tabs-mode nil)

;; Use y-or-n
(setq use-short-answers t)

;; Custom directory for backup files.
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backup" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "backup" user-emacs-directory) t)))

;; Do not display these buffers.
(add-to-list 'display-buffer-alist '("\\*mbsync\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

;; I now want compilation to show up.
;; (add-to-list 'display-buffer-alist '("\\*compilation\\*"
;;                                      display-buffer-no-window
;;                                      ((allow-no-window . t))))
(add-to-list 'display-buffer-alist '("\\*compilation\\*"
                                     display-buffer-in-previous-window))

(add-to-list 'display-buffer-alist '("\\*Async Shell Command\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

(add-to-list 'display-buffer-alist '("\\*rsync\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

(add-to-list 'display-buffer-alist '("\\*v2ray\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

(add-to-list 'display-buffer-alist '("\\*discord\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

(add-to-list 'display-buffer-alist '("\\*pipewire\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

;; Password-Store always fuck up at displaying at a correct window.
(add-to-list 'display-buffer-alist '("\\*Password-Store\\*"
                                     display-buffer-same-window))

(add-to-list 'display-buffer-alist '("\\*shell\\*"
                                     display-buffer-same-window))

(add-to-list 'display-buffer-alist '("\\*w3m\\*"
                                     display-buffer-in-previous-window))

(add-to-list 'display-buffer-alist '("\\*sly-mrepl.*\\*"
                                     display-buffer-same-window))

;; This is extremely annoying when compiling native elisp.
(setq warning-minimum-level :emergency)

(setq gc-cons-threshold 50000000)

(setq comint-pager "cat")

;; Add color in compilation buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(add-hook 'shell-mode-hook (lambda () (ansi-color-for-comint-mode-on)))

(setq use-package-always-ensure t)

(provide 'init-ui)
;;; init-ui.el ends here
