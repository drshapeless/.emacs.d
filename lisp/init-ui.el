;;; init-ui.el --- Personal preferences on user interface.
;;; Commentary:

;; User Interface settings.

;;; Code:

;; Use menlo as default font.
(set-frame-font "menlo" nil t)
(defun drsl/set-font-size ()
  (lambda () (set-face-attribute 'default nil :height 120)))
(set-face-attribute 'default nil :height 120)
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(setq after-load-theme-hook (lambda () (set-face-attribute 'default nil :height 120)))
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
(setq warning-minimum-level :emergency)


;; From https://www.emacswiki.org/emacs/IbufferMode.
(defun ajv/human-readable-file-sizes-to-bytes (string)
  "Convert a human-readable file size into bytes."
  (interactive)
  (cond
   ((string-suffix-p "G" string t)
    (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "M" string t)
    (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "K" string t)
    (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
   (t
    (string-to-number (substring string 0 (- (length string) 1))))
   )
  )

(defun ajv/bytes-to-human-readable-file-sizes (bytes)
  "Convert number of bytes to human-readable file size."
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes)))
  )

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size"
         :inline t
         :summarizer
         (lambda (column-strings)
           (let ((total 0))
             (dolist (string column-strings)
               (setq total
                     ;; like, ewww ...
                     (+ (float (ajv/human-readable-file-sizes-to-bytes string))
                        total)))
             (ajv/bytes-to-human-readable-file-sizes total)))	 ;; :summarizer nil
         )
  (ajv/bytes-to-human-readable-file-sizes (buffer-size)))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 20 20 :left :elide)
              " "
              (size-h 11 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))
(provide 'init-ui)
;;; init-ui.el ends here
