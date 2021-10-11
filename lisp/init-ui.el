;;; init-ui.el --- Personal preferences on user interface.
;;; Commentary:

;; User Interface settings.

;;; Code:

;; Use menlo as default font.
(set-frame-font "menlo" nil t)
(set-face-attribute 'default nil :height 120)
(if *is-linux*
    (progn
      (set-fontset-font t 'han "PingFang HK Regular")
      ;; Reduce blue light from screen.
      (shell-command "redshift -O 4500K")

      ;; Disable screen save.
      (shell-command "xset s off")
      (shell-command "xset -dpms")
      ))

(setq inhibit-startup-screen t)		; Disable startup screen.
(menu-bar-mode -1)			; Disable menu bar.
(scroll-bar-mode -1)			; Disable scroll bar.
(tool-bar-mode -1)			; Disable tool bar.

(setq display-time-format "%Y-%m-%d %H:%M")

;; This line prevent the mode line from showing useless CPU usage.
(setq display-time-load-average-threshold 10.0)
(display-time-mode t)			; Display time.

(column-number-mode t)
(blink-cursor-mode -1)			; Stop the stupid cursor from
                                        ; blinking
(global-visual-line-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(setq ring-bell-function 'ignore)	; No notification sound.
;; (global-auto-revert-mode t)		; Auto revert. It doesn't work
                                        ; well with tramp.

(set-frame-parameter nil 'fullscreen 'fullboth)

;; Startup buffer
;; (setq initial-buffer-choice "~/Documents/JackyLi.org")

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

;; This is extremely annoying when compiling native elisp.
(add-to-list 'display-buffer-alist '("\\*Warnings\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

(provide 'init-ui)
;;; init-ui.el ends here
