;;; init-ui.el --- User interface config. -*- lexical-binding: t -*-

;;; Commentary:

;; User interface settings.

;;; Code:

;; Use menlo as default font.
;; (set-frame-font "menlo" nil t)
(defun drsl/set-font-size ()
  (lambda () (set-face-attribute 'default nil :height 120)))
;; (set-face-attribute 'default nil :height 120)
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(setq after-load-theme-hook (lambda () (set-face-attribute 'default nil :height 120)))

(setq use-default-font-for-symbols nil)

(defun drsl/init-font ()
  (progn
    (set-face-font
     'default
     (font-spec
      :family "menlo"
      :size 18
      :weight 'normal
      :width 'normal
      :slant 'normal))
    (set-face-attribute 'default nil :height 120)

    (set-fontset-font
     t 'han
     (font-spec
      :family "PingFang HK"
      :size 18
      :weight 'normal
      :width 'normal
      :slant 'normal))

    (set-fontset-font
     t 'symbol
     (font-spec
      :family "Noto Color Emoji"
      :size 18
      :weight 'normal
      :width 'normal
      :slant 'normal))))

(if *is-a-linux*
    (progn
      ;; (set-fontset-font t 'han "PingFang HK Regular")
      (drsl/init-font)
      ;; Reduce blue light from screen.
      (shell-command "redshift -O 3600K")

      ;; Disable screen save.
      (shell-command "xset s off")
      (shell-command "xset -dpms")
      ))

(setq inhibit-startup-screen t)		; Disable startup screen.
(menu-bar-mode -1)                ; Disable menu bar.
(scroll-bar-mode -1)              ; Disable scroll bar.
(tool-bar-mode -1)                ; Disable tool bar.

(setq display-time-format "%Y-%m-%d %H:%M")

(setq-default tab-width 2)

;; This line prevent the mode line from showing useless CPU usage.
(setq display-time-load-average-threshold 10.0)
(display-time-mode t)                   ; Display time.

(column-number-mode t)
(blink-cursor-mode -1)                  ; Stop the stupid cursor from
                                        ; blinking
(global-visual-line-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(setq ring-bell-function 'ignore)	; No notification sound.
;; (global-auto-revert-mode t)		; Auto revert. It doesn't work
                                        ; well with tramp.

(set-frame-parameter nil 'fullscreen 'fullboth)

;; Cleanup whitespace before saving.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Enable line number mode in programming modes.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Never use tab.
(setq-default indent-tabs-mode nil)

;; Custom directory for backup files.
(setq backup-directory-alist
      `((".*" . ,"~/.emacs-backups")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs-backups" t)))

;; Do not display these buffers.
(add-to-list 'display-buffer-alist '("\\*mbsync\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

(add-to-list 'display-buffer-alist '("\\*compilation\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

(add-to-list 'display-buffer-alist '("\\*Async Shell Command\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

(add-to-list 'display-buffer-alist '("\\*rsync\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

;; Password-Store always fuck up at displaying at a correct window.
(add-to-list 'display-buffer-alist '("\\*Password-Store\\*"
                                     display-buffer-same-window))

;; This is extremely annoying when compiling native elisp.
(setq warning-minimum-level :emergency)

(provide 'init-ui)
;;; init-ui.el ends here
